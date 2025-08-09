#' Download and Import Taxonomic Data from Flora and Funga of Brazil
#'
#' This function downloads the latest version of the Darwin Core Archive (DwC-A) from the official
#' Flora and Funga of Brazil repository, extracts and processes taxonomic, distribution, and species
#' profile information. The result is returned as a cleaned and optionally simplified `data.frame`.
#'
#' @param dir Character. The directory where the DwC-A file will be downloaded and extracted.
#'            Default is a temporary directory created with `tempdir()`.
#' @param simply Logical. If `TRUE`, removes some columns related to taxonomic hierarchy and remarks
#'               (e.g., phylum, class, order, occurrenceRemarks) to simplify the output. Default is `TRUE`.
#'
#' @return A `data.frame` containing taxonomic information from Flora and Funga of Brazil. Columns include:
#' \itemize{
#'   \item \code{id} - Unique identifier for each taxon
#'   \item \code{family}, \code{genus}, \code{taxa}, \code{scientificName}, \code{acceptedNameUsage}
#'   \item \code{taxonomicStatus}, \code{nomenclaturalStatus}, \code{Endemismo}, \code{lifeForm}, and more
#' }
#' The exact columns may vary depending on the `simply` argument.
#'
#' @details
#' The function processes three files from the Darwin Core Archive:
#' \itemize{
#'   \item `taxon.txt`: Main taxonomic data
#'   \item `distribution.txt`: Information about geographic distribution and endemism
#'   \item `speciesprofile.txt`: Life form and other traits
#' }
#' It also filters for Plantae and constructs species names using taxonomic rank information.
#'
#' @note This function requires internet access and may take several seconds to run.
#'
#' @examples
#' \dontrun{
#' flora <- getReflora(cores = 5)
#' head(flora)
#' }
#'
#' @import data.table
#' @import parallel
#' @import jsonlite
#' @export
getReflora <- function(x, dir = tempdir(), simplify = TRUE, cores = 3) {
  require(jsonlite)
  require(parallel)
  url_dwca <- "https://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil"
  dir <- dir
  zip_file <- file.path(dir, "flora_brasil_dwca.zip")
  download.file(url_dwca, destfile = zip_file, mode = "wb")

  unzip(zip_file, exdir = dir)
  message("Download e extração concluídos em: ", dir)

  taxon_file <- file.path(dir, "taxon.txt")
  distr_file <- file.path(dir, "distribution.txt")
  profile_file <- file.path(dir, "speciesprofile.txt")

  if (!file.exists(taxon_file)) {
    stop("Arquivo taxon.txt não encontrado na pasta fornecida.")
  }

  taxa <- data.table::fread(taxon_file, showProgress = F, verbose = F)
  profile <- data.table::fread(profile_file, showProgress = F, verbose = F)
  distr <- data.table::fread(distr_file, showProgress = F, verbose = F)

  plantae <- taxa[grep(x = taxa$kingdom, pattern = "Plantae|PLANTAE"), ]

  spp <- !grepl(
    plantae$taxonRank,
    pattern = "FAMILIA|GENERO|TRIBO|SUB_FAMILIA|CLASSE"
  )

  plantae[spp, ]

  plantae$taxa[spp] <-
    paste0(
      plantae$genus[spp],
      " ",
      plantae$specificEpithet[spp],
      " ",
      ifelse(
        plantae$taxonRank[spp] == "VARIEDADE",
        "var.",
        ifelse(
          plantae$taxonRank[spp] == "SUB_ESPECIE",
          "subsp.",
          ifelse(
            plantae$taxonRank[spp] == "FORMA",
            "f.",
            ""
          )
        )
      ),
      " ",
      plantae$infraspecificEpithet[spp]
    )
  plantae$taxa <- trimws(plantae$taxa)

  d <-
    plantae[
      grepl(
        x = plantae$taxonRank,
        pattern = "FAMILIA|GENERO|TRIBO|SUB_FAMILIA|CLASSE"
      ),
    ]$scientificName

  plantae[
    grepl(
      x = plantae$taxonRank,
      pattern = "FAMILIA|GENERO|TRIBO|SUB_FAMILIA|CLASSE"
    ),
  ]$taxa <-
    gsub(d, pattern = " .*", replacement = "")

  taxa_df <- data.frame(
    id = plantae$id,
    phylum = plantae$phylum,
    class = plantae$class,
    order = plantae$order,
    family = plantae$family,
    genus = plantae$genus,
    taxa = plantae$taxa,
    scientificName = plantae$scientificName,
    acceptedNameUsage = plantae$acceptedNameUsage,
    taxonomicStatus = plantae$taxonomicStatus,
    nomenclaturalStatus = plantae$nomenclaturalStatus,
    stringsAsFactors = FALSE
  )

  rows <- !is.na(profile$lifeForm) & profile$lifeForm != ""
  profile$Form[rows] <- parallel::mclapply(profile$lifeForm[rows], mc.cores = cores, function(x) fromJSON(x)$lifeForm)
  profile$Habitat[rows] <- parallel::mclapply(profile$lifeForm[rows], mc.cores = cores, function(x) fromJSON(x)$habitat)
  profile$VegType[rows] <- parallel::mclapply(profile$lifeForm[rows], mc.cores = cores, function(x) fromJSON(x)$vegetationType)
  profile$lifeForm <- NULL
  profile$habitat <- NULL

  rows <- !is.na(distr$occurrenceRemarks) & distr$occurrenceRemarks != ""
  distr$Endemism[rows] <-
    parallel::mclapply(distr$occurrenceRemarks[rows],
      mc.cores = cores, function(x) fromJSON(x)$endemism
    )
  distr$Domain[rows] <-
    parallel::mclapply(distr$occurrenceRemarks[rows],
      mc.cores = cores, function(x) fromJSON(x)$phytogeographicDomain
    )

  distr$occurrenceRemarks <- NULL

  dist_df <- merge(taxa_df, distr, by = "id", all.x = TRUE)
  final_df <- merge(dist_df, profile, by = "id", all.x = TRUE)
  final_df <- final_df[!duplicated(final_df$id), ]

  colnames(final_df)
  if (simplify == TRUE) {
    cols_to_remove <- c("occurrenceRemarks", "countryCode", "phylum", "class", "order")
    final_df <- final_df[, !(names(final_df) %in% cols_to_remove)]
  }
  return(final_df)
}
