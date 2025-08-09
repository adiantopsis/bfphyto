#' @title Matching species names with a species df
#'
#' @description
#' Performs approximate (fuzzy) matching between a list of species names and a reference list, using `agrep` with parallel processing.
#' The main goal of this function is to identify threatened species in your dataset based on the reference data provided within the package (`ameacadas_rs`, `ameacadas_br`, `ameacadas_br_cnc2020`, `plantas_raras_br`).
#'
#' @param species A character vector with the species names to be matched.
#' @param species_to_match A data frame of threaten species with at least one columns: `spp` (reference species names).
#' @param max.distance A numeric value between 0 and 1 indicating the maximum allowed distance for matching (default: 0.05).
#' @param ncores The number of cores to use for parallel processing (default: 1).
#'
#' @return
#' A data frame containing, for each matched input species:
#' \itemize{
#'   \item \code{input_name}: the original species name provided.
#'   \item \code{Suggested_name}: the best-matched species name from the reference list.
#'   \item \code{Distance}: the string edit distance between the input and suggested name.
#'   \item \code{Categoria}: the associated category of the matched species.
#' }
#'
#' @details
#' This function uses `agrep` internally to perform fuzzy matching and leverages `doParallel` for multi-core processing. Only input species with at least one match are included in the result.
#'
#' @import foreach
#' @import doParallel
#' @importFrom tidyr drop_na
#' @importFrom dplyr %>%
#'
#' @examples
#' \dontrun{
#' data(ameacadas_rs)
#' species <- c("Araucaria angustifolia", "Hippeastrum breviflorum")
#' species_to_match <- ameacadas_rs
#' getThreat(species, species_to_match, max.distance = 0.1, ncores = 2)
#' }
#'
#' @export
getSpp <- function(species, species_to_match, max.distance = 0.05, ncores = 1) {
  if (!requireNamespace("foreach", quietly = TRUE) || !requireNamespace("doParallel", quietly = TRUE)) {
    stop("Packages 'foreach' and 'doParallel' are required but not installed.")
  }
  spp <- grep(colnames(species_to_match), pattern = "spp|Especies|Espécies|Especie|Espécie|Species|species")
  cat <- grep(colnames(species_to_match), pattern = "cat|categoria|Categoria|Category|category")
  # Setup parallel backend
  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)

  if (!is.vector(species_to_match)) {
    # Run parallel fuzzy matching
    sp_l <- foreach::foreach(x = seq_along(species), .combine = rbind) %dopar% {
      matched_names <- agrep(species[x], species_to_match[, spp],
        value = TRUE, max.distance = max.distance
      )
      if (length(matched_names) > 0) {
        distances <- as.numeric(adist(species[x], matched_names))
        categorias <- species_to_match[match(matched_names, species_to_match[, spp]), cat]

        data.frame(
          input_name = species[x],
          Suggested_name = matched_names,
          Distance = distances,
          Categoria = categorias,
          stringsAsFactors = FALSE
        )
      } else {
        NULL
      }
    }
  } else {
    sp_l <- foreach::foreach(x = seq_along(species), .combine = rbind) %dopar% {
      matched_names <- agrep(species[x], species_to_match,
        value = TRUE, max.distance = max.distance
      )
      if (length(matched_names) > 0) {
        distances <- as.numeric(adist(species[x], matched_names))

        data.frame(
          input_name = species[x],
          Suggested_name = matched_names,
          Distance = distances,
          stringsAsFactors = FALSE
        )
      } else {
        NULL
      }
    }

    parallel::stopCluster(cl)

    # Handle case where no matches are found
    if (is.null(sp_l)) {
      return(data.frame(
        input_name = character(),
        Suggested_name = character(),
        Distance = numeric(),
        Categoria = character(),
        stringsAsFactors = FALSE
      ))
    }
  }
  return(sp_l)
}
