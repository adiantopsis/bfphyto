#' @title Get data on Species Link
#'
#' @description
#' This function interfaces with the [speciesLink](https://specieslink.net/) web service to retrieve occurrence data
#' for plant, animal, microorganism, or fossil specimens. It allows fine filtering using taxonomic, geographic, and metadata fields.
#' The output can be optionally saved in `.csv` or `.rds` formats.
#'
#' @param apikey A character string with your speciesLink API key. If `NULL`, a message is shown with the link to get one.
#' @param dir Directory where output files will be saved (if `save = TRUE`).
#' @param filename Name of the output file (without extension).
#' @param save Logical. If `TRUE`, the resulting data will be saved to disk.
#' @param basisOfRecord Type of record: `"PreservedSpecimen"`, `"LivingSpecimen"`, `"FossilSpecimen"`, `"HumanObservation"`, `"MachineObservation"`, `"MaterialSample"`.
#' @param family Character vector with family names to filter the query.
#' @param species Character vector with species scientific names (up to 50).
#' @param collectionCode Character vector with collection codes.
#' @param country, stateProvince, county Geographic filters. Character vectors with names.
#' @param bbox Numeric vector defining a bounding box in the format: `c(minLon, minLat, maxLon, maxLat)`.
#' @param Coordinates Character. Options: `"yes"`, `"no"`, `"consistent"`, `"original"`, `"automatic"`, `"blocked"`.
#' @param CoordinatesQuality Character. `"Good"` or `"Bad"`.
#' @param Scope Character. Data scope: `"plants"`, `"animals"`, `"microrganisms"`, or `"fossils"`.
#' @param Synonyms Synonym database: `"species2000"`, `"flora2020"`, `"MycoBank"`, `"AlgaeBase"`, `"DSMZ"`, or `"no synonyms"`.
#' @param Typus Logical. If `TRUE`, restricts to typus specimens.
#' @param Images Character. Options: `"Yes"`, `"Live"`, `"Polen"`, `"Wood"`.
#' @param RedList Logical or character. If `TRUE` or `"all"`, filters Red List species (e.g., `"VU+EN+CR"`).
#' @param MaxRecords Integer. Maximum number of records to return.
#' @param file.format File format to save the data: `"csv"` (default) or `"rds"`.
#' @param compress Logical. Whether to compress the output file.
#'
#' @return A data frame containing species occurrence data returned by the speciesLink web service. If `save = TRUE`, also saves the data to disk.
#'
#' @details
#' This function builds a query URL using selected filters, sends the request to speciesLink,
#' parses the JSON response, and returns the occurrence records. It supports saving results locally.
#'
#' @note
#' Ensure that you respect all data usage and citation policies from speciesLink and its data providers.
#'
#' @seealso \url{https://specieslink.net}, \url{https://specieslink.net/ws/1.0/search}
#'
#' @examples
#' \dontrun{
#' getSplink(
#'   apikey = "your_api_key_here",
#'   species = c("Euterpe edulis"),
#'   country = "Brazil",
#'   save = TRUE,
#'   file.format = "csv"
#' )
#' }
#'
#' @export
getSplink <- function(apikey = NULL,
                      dir = "results/",
                      filename = "output",
                      save = FALSE,
                      basisOfRecord = NULL,
                      family = NULL,
                      species = NULL,
                      collectionCode = NULL,
                      country = NULL,
                      stateProvince = NULL,
                      county = NULL,
                      bbox = NULL,
                      Coordinates = NULL, # 		Yes | No | Original | Automatic | Blocked
                      CoordinatesQuality = NULL, # Good | Bad
                      Scope = NULL, # 			plants, animals, microrganisms,fossils
                      Synonyms = "no synomyms", # species2000 | flora2020 | MycoBank | AlgaeBase | DSMZ | Moure no synonyms
                      Typus = FALSE,
                      Images = NULL,
                      RedList = FALSE,
                      MaxRecords = NULL, # 		n > 0	 all records
                      file.format = "csv",
                      compress = FALSE) { # Yes | No | Live | Polen | Wood
  # speciesLink url
  my_url <- "https://specieslink.net/ws/1.0/search?"
  if (is.null(apikey)) {
    message("Please insert apikey OR
visit: https://specieslink.net/aut/login/?next=/aut/profile/apikeys to get one")
  } else {
    apikey <- paste0("apikey=", apikey)
  }


  # helper function
  url_query <- function(vector, name) {
    char <- paste(paste0(vector, "&"), collapse = "")
    url <- paste0(name, "=", char)
    return(url)
  }

  # basis of record
  if (is.null(basisOfRecord)) {
    my_url
  } else {
    if (basisOfRecord %in% c(
      "PreservedSpecimen",
      "LivingSpecimen",
      "FossilSpecimen",
      "HumanObservation",
      "MachineObservation",
      "MaterialSample"
    )) {
      br <- url_query(basisOfRecord, "basisOfRecord")
      my_url <- paste0(my_url, br)
    }
  }
  # Species name
  if (is.null(species)) {
    my_url
  } else {
    if (is.character(species)) {
      if (length(species) > 50) {
        stop("Please make request of no more than 50 species at a time!")
      }
      species <- gsub(" ", "+", species)
      sp <- url_query(species, "scientificName")
      my_url <- paste0(my_url, sp)
    } else {
      stop("species must be a character")
    }
  }
  # Family name
  if (is.null(family)) {
    my_url
  } else {
    if (is.character(family)) {
      fam <- url_query(family, "family")
      my_url <- paste0(my_url, fam)
    } else {
      stop("family name must be a character")
    }
  }
  # Collection code
  if (is.null(collectionCode)) {
    my_url
  } else {
    if (is.character(collectionCode)) {
      cc <- url_query(collectionCode, "collectionCode")
      my_url <- paste0(my_url, cc)
    }
  }
  # country
  if (is.null(country)) {
    my_url
  } else {
    if (is.character(country)) {
      country <- gsub(" ", "+", country)
      ct <- url_query(country, "country")
      my_url <- paste0(my_url, ct)
    }
  }
  # stateProvince
  if (is.null(stateProvince)) {
    my_url
  } else {
    if (is.character(stateProvince)) {
      stateProvince <- gsub(" ", "+", stateProvince)
      st <- url_query(stateProvince, "stateProvince")
      my_url <- paste0(my_url, st)
    }
  }
  # county
  if (is.null(county)) {
    my_url
  } else {
    if (is.character(county)) {
      county <- gsub(" ", "+", county)
      co <- url_query(county, "county")
      my_url <- paste0(my_url, co)
    }
  }
  # bbox
  if (is.null(bbox)) {
    my_url
  } else {
    bb <- paste0(bbox, collapse = "+")
    bb <- url_query(bb, "bbox")
    my_url <- paste0(my_url, bb)
  }

  # Coordinates
  if (is.null(Coordinates)) {
    my_url
  } else {
    if (Coordinates %in% c("yes", "no", "consistent", "original", "suspect", "automatic", "blocked")) {
      xy <- url_query(Coordinates, "Coordinates")
      my_url <- paste0(my_url, xy)
    }
  }
  # Coordinates quality
  if (is.null(CoordinatesQuality)) {
    my_url
  } else {
    if (CoordinatesQuality %in% c("Good", "Bad")) {
      cq <- url_query(CoordinatesQuality, "CoordinatesQuality")
      my_url <- paste0(my_url, cq)
    }
  }

  # Scope
  if (is.null(Scope)) {
    my_url
  } else {
    if (Scope %in% c("plants", "animals", "microrganisms", "fossils")) {
      sc <- url_query(abbreviate(Scope, minlength = 1), "Scope")
      my_url <- paste0(my_url, sc)
    }
  }
  #  Synonyms
  # if (length(species) > 9) {
  #    stop("Function does not support synonym check of more than nine species")
  #  } else {
  if (is.null(Synonyms)) {
    my_url
  } else {
    if (Synonyms %in% c(
      "species2000", "flora2020",
      "MycoBank", "AlgaeBase", "DSMZ"
    )) {
      sy <- url_query(Synonyms, "Synonyms")
      my_url <- paste0(my_url, sy)
    }
  }
  #  }
  #  Typus
  if (Typus == FALSE) {
    my_url
  } else {
    my_url <- paste0(my_url, "Typus/Yes/")
  }
  # Images # "Yes", "Live", "Polen", "Wood"
  if (is.null(Images)) {
    my_url
  } else {
    if (Images %in% c("Yes", "Live", "Polen", "Wood")) {
      im <- url_query(Images, "Images")
      my_url <- paste0(my_url, im)
    }
  }
  # RedList
  if (RedList == FALSE) {
    my_url
  } else {
    if (RedList == "all") {
      RedList <- "VU+EN+CR+EX+RE+PEX+EW"
      rl <- url_query(RedList, "redlist")
      my_url <- paste0(my_url, rl)
    } else {
      rl <- url_query(RedList, "redlist")
      my_url <- paste0(my_url, rl)
    }
  }

  # MaxRecords
  if (is.null(MaxRecords)) {
    my_url
  } else {
    if (is.numeric(MaxRecords)) {
      mr <- url_query(MaxRecords, "MaxRecords")
      my_url <- paste0(my_url, mr)
    }
  }

  # making request
  my_url <- paste0(my_url, apikey)
  message("Making request to speciesLink...")



  # r <- httr::GET(my_url)
  # message("Extracting content ...")
  # rr <- httr::content(r, as="parse") # text content
  # requesting JSON format
  df <- jsonlite::fromJSON(my_url)$features$properties
  # rrr <- readr::read_tsv(rr, locale = readr::locale(encoding = "UTF-8"))

  if (save) {
    # creating dir
    if (!dir.exists(dir)) {
      dir.create(dir)
    }

    # fullname <- paste0(dir, filename, ".csv")
    # message(paste0("Writing ", fullname, " on disk."))
    # write.csv(df,
    #           fullname,
    #           row.names = FALSE)

    if (file.format == "csv") {
      if (compress) {
        fullname <- paste0(dir, filename, ".csv.zip")
        message(paste0("Writing ", fullname, " on disk."))
        data.table::fwrite(df, file = fullname, compress = "gzip")
      } else {
        fullname <- paste0(dir, filename, ".csv")
        message(paste0("Writing ", fullname, " on disk."))
        data.table::fwrite(df, file = fullname)
      }
    }

    if (file.format == "rds") {
      fullname <- paste0(dir, filename, ".rds")
      message(paste0("Writing ", fullname, " on disk."))

      if (compress) {
        saveRDS(df, file = fullname, compress = "gzip")
      } else {
        saveRDS(df, file = fullname, compress = FALSE)
      }
    }
  }
  # if output is empty, return message
  if (is.null(dim(df))) {
    warning("Output is empty. Check your request.")
  }

  warning("Please make sure that the restrictions and citation indicated by
  each speciesLink/CRIA data provider are observed and respected.")


  return(df)
}
