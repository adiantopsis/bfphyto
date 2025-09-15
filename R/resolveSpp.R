#' Resolve Species Names Using Flora and Funga of Brazil
#'
#' This function correct your list of plant species names based on Flora and Funga of Brazil <https://floradobrasil.jbrj.gov.br/consulta/#CondicaoTaxonCP>
#' using exact and fuzzy string matching. It returns a cleaned and enriched data frame with taxonomic
#' status, accepted names, endemism, and origin.
#'
#' @param species Character vector. List of species names to be resolved. Names should follow the botanical
#'                binomial convention (e.g., "Araucaria angustifolia"), morphospecies nomination is allowed (e.g., Cyperus sp. 1 or Asteraceae sp. 1)
#'                but will be concatenated in the final data frame.
#' @param flora A `data.frame` returned by [getReflora()]. If not provided, the function will call `getReflora()`
#'              to download and process the most recent dataset. Default is `getReflora()`.
#'
#' @param simplify Logical. If `TRUE` return few columns with species submitted, matched and acceptedname.
#' If `FALSE` return all previous information along with species origin, habitat, lifeform or its occurrence domain.
#'
#' @return A `data.frame` with taxonomic resolution results, including the following columns (if available):
#' \itemize{
#'   \item \code{Family} — Family name
#'   \item \code{Genus} — Genus name
#'   \item \code{SubmittedName} — Original species name submitted by the user
#'   \item \code{MatchedName} — Closest matching name found in the flora
#'   \item \code{AcceptedName} — Accepted name based on taxonomic status (2 or 3-word version)
#'   \item \code{MatchType} — Match type: "Exact", "Fuzzy", or "Not Found"
#'   \item \code{Status} — Taxonomic status (e.g., "Accepted Name" or "Synonym")
#'   \item \code{Endemism} — Endemism status, if available (e.g., "Endêmica", "Não Endêmica")
#'   \item \code{Origin} — Return if species is native, cultivated or naturalized
#'   \item \code{VegType} — Brazilian phytogeographic domain of occurrence
#' }
#'
#' @details
#' The function correct performs:
#' \itemize{
#'   \item Exact matching
#'   \item Fuzzy matching (with `max.distance = 0.1`)
#' }
#' @note For large species lists, fuzzy matching may take a few seconds to process.
#'
#' ALWAYS submit scientific names to minimize the errors.
#' ALWAYS check the final results looking if the submitted names matched correctly
#'
#' @examples
#' \dontrun{
#' # Load the Flora and Funga of Brazil dataset
#' flora <- getReflora()
#'
#' # Example species list (Southern_Camp$spp must be available in your environment)
#' my_spp <- resolveSpp(Southern_Camp$spp, flora = getReflora())
#' head(my_spp)
#' }
#'
#' @seealso [getReflora()]
#'
#' @export
resolveSpp <- function(species, flora = getReflora(), simplify = FALSE) {
  capitalize <- function(x) {
    sapply(
      strsplit(x, " "),
      function(y) {
        paste(
          toupper(substring(y, 1, 1)),
          tolower(substring(y, 2)),
          sep = "",
          collapse = " "
        )
      },
      USE.NAMES = FALSE
    )
  }
  species <- trimws(gsub(
    pattern = "\\bsp\\b.*",
    replacement = " ",
    x = species
  ))

  exact_df <- flora[flora$taxa %in% species, ]
  tryCatch(exact_df$MatchType <- "Exact", error = function(e) {
    rep(NA, ncol(flora))
  })

  exact_df$SubmittedName <- exact_df$taxa
  not_matched <- setdiff(species, flora$taxa)
  fuzzy_matches <- lapply(not_matched, function(y) {
    candidatos <- agrep(
      pattern = y,
      x = flora$taxa,
      max.distance = 0.1,
      value = TRUE
    )
    if (grepl("var\\.|subsp\\.", y)) {
      candidatos <- candidatos[grepl("var\\.|subsp\\.", candidatos)]
    } else {
      candidatos <- candidatos[!grepl("var\\.|subsp\\.", candidatos)]
    }
    if (length(candidatos) == 0) NA else candidatos[1]
  })
  names(fuzzy_matches) <- not_matched
  fuzzy_df <- data.frame(
    SubmittedName = names(fuzzy_matches),
    MatchedName = unlist(fuzzy_matches),
    stringsAsFactors = FALSE
  )
  fuzzy_found <- flora[flora$taxa %in% fuzzy_df$MatchedName, ]
  tryCatch(fuzzy_found$MatchType <- "Fuzzy", error = function(e) {
    rep(NA)
  })
  fuzzy_found$SubmittedName <- fuzzy_df$SubmittedName[match(
    fuzzy_found$taxa,
    fuzzy_df$MatchedName
  )]
  not_found_n <- sum(is.na(fuzzy_df$MatchedName))
  not_found_df <- if (not_found_n > 0) {
    nf <- data.frame(matrix(NA, nrow = not_found_n, ncol = ncol(flora)))
    colnames(nf) <- colnames(flora)
    nf$MatchType <- "Not Found"
    nf$acceptedNameUsage <- fuzzy_df$SubmittedName[is.na(fuzzy_df$MatchedName)]
    nf$SubmittedName <- nf$acceptedNameUsage
    nf
  } else {
    NULL
  }
  df <- rbind(fuzzy_found, exact_df, not_found_df)
  idx_vazios <- is.na(df$acceptedNameUsage) | df$acceptedNameUsage == ""
  df$acceptedNameUsage[idx_vazios] <- df$taxa[idx_vazios]
  df$Genus <- sub(" .*", "", df$acceptedNameUsage)
  df$MatchedName <- df$taxa
  df$AcceptedName <- ifelse(
    grepl(" var\\.| subsp\\.| x ", tolower(df$acceptedNameUsage)),
    sub("^(([^ ]+ +){4})[^ ]+.*$", "\\1", df$acceptedNameUsage),
    sub("^(([^ ]+ +){2})[^ ]*.*$", "\\1", df$acceptedNameUsage)
  )
  df$AcceptedName <- trimws(df$AcceptedName)
  df$AcceptedName[df$MatchType == "Not Found"] <- NA
  df$Genus[df$MatchType == "Not Found"] <- NA
  df$Genus[df$establishmentMeans == "Not Found"] <- NA
  df$Family <- df$family
  cor <- flora[
    flora$taxa %in% df$AcceptedName & flora$taxonomicStatus == "NOME_ACEITO",
  ]
  idx_sinonimos <- which(
    df$taxonomicStatus != "NOME_ACEITO" &
      !is.na(df$AcceptedName)
  )
  match_idx <- match(df$AcceptedName[idx_sinonimos], cor$taxa)
  validos <- which(!is.na(match_idx))
  cols_para_substituir <- c("establishmentMeans", "Endemism")
  cols_validas <- cols_para_substituir[
    cols_para_substituir %in%
      names(df) &
      cols_para_substituir %in% names(cor)
  ]
  for (col in cols_validas) {
    df[idx_sinonimos[validos], col] <- cor[match_idx[validos], col]
  }
  colnames(df)
  df$Origin[!is.na(df$establishmentMeans)] <- capitalize(df$establishmentMeans[
    !is.na(df$establishmentMeans)
  ])
  df$Status[!is.na(df$taxonomicStatus)] <- capitalize(gsub(
    df$taxonomicStatus[!is.na(df$taxonomicStatus)],
    pattern = "_",
    replacement = " "
  ))
  df$Distance <- mapply(adist, x = df$SubmittedName, y = df$MatchedName)
  colnames(df)
  if (simplify == TRUE) {
    colunas_finais <- c(
      "Family",
      "SubmittedName",
      "MatchedName",
      "AcceptedName",
      "Distance",
      "MatchType"
    )
  } else {
    colunas_finais <- c(
      "Family",
      "Genus",
      "SubmittedName",
      "MatchedName",
      "AcceptedName",
      "Distance",
      "MatchType",
      "Status",
      "Endemism",
      "Origin",
      "Habitat",
      "Form",
      "VegType"
    )
  }
  df <- df[, colunas_finais[colunas_finais %in% names(df)]]
  return(df)
}
