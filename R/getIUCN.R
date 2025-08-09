#' @title Retrieve IUCN Conservation Status for a List of Species
#'
#' @description
#' The `getIUCN` function queries the IUCN Red List API to retrieve the conservation status of a list of species.
#' It returns the IUCN threat category (e.g., "CR", "EN", "VU") for each valid species provided in the input.
#'
#' @param x A character vector containing species names to query.
#' @param api_key A valid API key for the IUCN Red List API. You can obtain a key from \url{https://apiv3.iucnredlist.org/api/v3/token}.
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{input_name}{The species name submitted in the query.}
#'   \item{IUCN}{The corresponding IUCN Red List category code (e.g., "CR", "EN", "VU").}
#' }
#'
#' @details
#' This function uses the `taxize::iucn_summary()` function to access the IUCN API.
#' Only species for which a valid IUCN status is found are included in the result.
#'
#' @note This function requires an internet connection and a valid IUCN API key.
#' Results depend on the availability and accuracy of the IUCN Red List database.
#'
#' @seealso \code{\link[taxize]{iucn_summary}}
#'
#' @importFrom taxize iucn_summary
#'
#' @examples
#' \dontrun{
#' species <- c("Panthera onca", "Ateles belzebuth", "Cebus apella")
#' api_key <- "your_api_key_here"
#' getIUCN(species, api_key)
#' }
#'
#' @export
getIUCN <- function(x, api_key) {
  if (!requireNamespace("taxize", quietly = TRUE)) {
    stop("The 'taxize' package is required but not installed.")
  }

  extract_code <- function(species) {
    if (is.list(species) && !is.null(species$red_list_category$code)) {
      return(species$red_list_category$code)
    } else if (is.character(species)) {
      return(species)
    } else {
      return(NA_character_)
    }
  }

  # Call the IUCN API for each species
  species_status <- taxize::iucn_summary(x = x, key = api_key, distr_detail = TRUE)

  # Extract the conservation codes
  species_codes <- sapply(species_status, extract_code)

  # Remove NAs and return result as data.frame
  valid_idx <- !is.na(species_codes)

  result <- data.frame(
    input_name = names(species_codes)[valid_idx],
    IUCN = species_codes[valid_idx],
    stringsAsFactors = FALSE
  )

  return(result)
}
