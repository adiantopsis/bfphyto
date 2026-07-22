#' Download and Process POWO/WCVP Distribution Data
#'
#' Downloads and processes data from the World Checklist of
#' Vascular Plants (WCVP), made available by Kew, returning taxonomic and
#' geographic distribution information for specified families, genera, or
#' species. Uses the botanical regions from the \pkg{expowo} package to
#' associate each occurrence record with its corresponding botanical region
#' and country.
#'
#' @param families Character vector. Name(s) of family/families to filter
#'   by. If \code{NULL} (default), no family filter is applied unless
#'   \code{genus} or \code{species} are provided.
#' @param genus Character vector. Name(s) of genus/genera to filter by.
#'   Takes priority over \code{families} when provided. Default \code{NULL}.
#' @param species Character vector. Full scientific name(s) of species to
#'   filter by. Takes priority over \code{genus} and \code{families} when
#'   provided. Default \code{NULL}.
#' @param synonyms Logical. If \code{FALSE} (default), only records whose
#'   \code{taxonomicstatus} is \code{"Accepted"} are kept, removing
#'   synonyms from the final result. If \code{TRUE}, all taxonomic
#'   statuses are retained.
#' @param dir Character. Path to the directory where the WCVP files
#'   (\code{wcvp_taxon.csv} and \code{wcvp_distribution.csv}) are or will
#'   be saved. If the files already exist in this directory, the download
#'   step is skipped and the data is read directly. Default \code{tempdir()}.
#'
#' @details
#' This function was made because \code{expowo} package do not download ferns species
#' data occurrence data correctly. And its way of do things is not the most
#' intuitive to ecological research. This funcion in the first run 
#' (or whenever the files do not yet exist in \code{dir}),
#' the function downloads the Darwin Core Archive zip file from Kews
#' repository, extracts its contents, and loads the taxonomy
#' (\code{wcvp_taxon.csv}) and distribution (\code{wcvp_distribution.csv})
#' tables using \code{data.table::fread}.
#'
#' The taxonomic filter follows a priority order: \code{species} takes
#' precedence over \code{genus}, which in turn takes precedence over
#' \code{families}. If \code{species} is provided, the other filter
#' arguments are ignored; if \code{species} is \code{NULL} but
#' \code{genus} is provided, the filter is applied by genus; and so on.
#' If none of the three are provided, all taxa in the dataset are used.
#'
#' The \code{expowo} package is automatically installed if not already
#' available, since it provides the \code{botregions} object used to
#' classify each locality into a botanical region and country.
#'
#' @return A \code{data.frame} containing, for each occurrence record:
#'   \code{coreid}, \code{family}, \code{genus}, \code{scientfiicname},
#'   \code{scientfiicnameauthorship}, \code{taxonrank},
#'   \code{acceptednameusageid}, \code{taxonomicstatus}, \code{locality},
#'   \code{threatstatus}, \code{establishmentmeans}, \code{botanic_region},
#'   and \code{country}.
#'
#' @note The WCVP file download can be large (tens of MB) and prone to
#'   timeouts on slow connections; the function already sets
#'   \code{options(timeout = 800)} internally to mitigate this.
#'
#' @examples
#' \dontrun{
#' # Get all species from a specific family
#' df <- getPOWO(families = "Myrtaceae")
#'
#' # Get by genus, including synonyms
#' df <- getPOWO(genus = "Eugenia", synonyms = TRUE)
#'
#' # Get specific species
#' df <- getPOWO(species = c("Eugenia uniflora", "Psidium guajava"))
#' }
#'
#' @export
getPOWO <- function(families = NULL, genus = NULL, species = NULL, synonyms = FALSE,
                    dir = tempdir()){
  
  options(timeout = 800)
  
  if("expowo" %in% installed.packages()){  
    botregions <- expowo::botregions
  } else{
    install.packages("expowo")
    botregions <- expowo::botregions
  }
  
  
  dir <- dir
  taxon_file <- file.path(dir, "wcvp_taxon.csv")
  distr_file <- file.path(dir, "wcvp_distribution.csv")
  
  
  if (file.exists(taxon_file)) {
    taxa <- data.table::fread(taxon_file, showProgress = F, verbose = F)
    distr <- data.table::fread(distr_file, showProgress = F, verbose = F)
  } else {
    dir <- dir
    url_dwca <- "https://sftp.kew.org/pub/data-repositories/WCVP/wcvp_dwca.zip"
    zip_file <- file.path(dir, "powo_dwca.zip")
    download.file(url_dwca, destfile = zip_file, mode = "wb", method = "libcurl")
    unzip(zip_file, exdir = dir)
    message("Download e extração concluídos em: ", dir)
    taxa <- data.table::fread(taxon_file, showProgress = F, verbose = F)
    distr <- data.table::fread(distr_file, showProgress = F, verbose = F)
  }
  
  
  
  if(!is.null(species)){
    sub <- taxa[taxa$scientfiicname %in% species,]
  } else {
    if(!is.null(genus)){
      sub <- taxa[taxa$genus %in% genus,]
    } else{
      if(!is.null(families)){
        sub <- families
        sub <- taxa[taxa$family %in% families,]
      } else {
        sub <- taxa
      }
    }
  }
  
  sub$coreid <- sub$taxonid
  
  occ_sub <- distr[distr$coreid %in% sub$coreid,]
  
  union_occ <- merge(sub, occ_sub, by= "coreid") 
  
  
  infos_occ<- subset(union_occ, select = c(coreid,family, genus, scientfiicname, 
                                           scientfiicnameauthorship, taxonrank, 
                                           acceptednameusageid, taxonomicstatus,
                                           locality, threatstatus, establishmentmeans))
  
  infos_occ$botanic_region <- botregions$botanical_division[
    match(infos_occ$locality, botregions$botanical_division)
  ]
  
  infos_occ$country <- botregions$country[
    match(infos_occ$locality, botregions$country)
  ]
  
  view(infos_occ)
  
  if(synonyms == FALSE){
    infos_occ  <- infos_occ[infos_occ$taxonomicstatus == "Accepted",]
    
  }
  
  return(infos_occ)
}

