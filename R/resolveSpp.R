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
#' @return A `data.frame` with taxonomic resolution results, including the following columns (if available):
#' \itemize{
#'   \item \code{Family} — Family name
#'   \item \code{Genus} — Genus name
#'   \item \code{SubmittedName} — Original species name submitted by the user
#'   \item \code{MatchedName} — Closest matching name found in the flora
#'   \item \code{AcceptedName} — Accepted name based on taxonomic status (2 or 3-word version)
#'   \item \code{MatchType} — Match type: "Exact", "Fuzzy", or "Not Found"
#'   \item \code{Status} — Taxonomic status (e.g., "Nome Aceito", "Sinônimo") capitalized
#'   \item \code{Endemism} — Endemism status, if available (e.g., "Endêmica", "Não Endêmica")
#'   \item \code{Origin} — Establishment means (e.g., "Nativa", "Exótica")
#' }
#'
#' @details
#' The function performs:
#' \itemize{
#'   \item Exact matching using `%in%`
#'   \item Fuzzy matching using `agrep` (with `max.distance = 0.1`)
#'   \item Filtering of infra-specific ranks (`var.`, `subsp.`) to improve fuzzy matching
#'   \item Automatic replacement of data for synonyms with values from their accepted names
#'   \item Capitalization of taxonomic status and origin for readability
#' }
#'
#' @note For large species lists, fuzzy matching may take a few seconds to process.
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

  # Busca exata
  exact_df <- flora[flora$taxa %in% species, ]
  exact_df$MatchType <- "Exact"
  exact_df$SubmittedName <- exact_df$taxa

  # Espécies não encontradas exatamente
  not_matched <- setdiff(species, flora$taxa)

  # Busca fuzzy com controle de "var." ou "subsp."
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

  # Resultados fuzzy encontrados
  fuzzy_found <- flora[flora$taxa %in% fuzzy_df$MatchedName, ]
  fuzzy_found$MatchType <- "Fuzzy"
  fuzzy_found$SubmittedName <-
    fuzzy_df$SubmittedName[match(fuzzy_found$taxa, fuzzy_df$MatchedName)]

  # Resultados não encontrados
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

  # Combinar todos
  df <- rbind(fuzzy_found, exact_df, not_found_df)

  # Preencher acceptedNameUsage se estiver vazio
  idx_vazios <- is.na(df$acceptedNameUsage) | df$acceptedNameUsage == ""
  df$acceptedNameUsage[idx_vazios] <- df$taxa[idx_vazios]

  # Extrair gênero
  df$Genus <- sub(" .*", "", df$acceptedNameUsage)

  # Renomear coluna original
  df$MatchedName <- df$taxa

  # Criar acceptedname com 2 ou 3 palavras
  df$AcceptedName <- ifelse(
    grepl(" var\\.| subsp\\.| x ", tolower(df$acceptedNameUsage)),
    sub("^(([^ ]+ +){4})[^ ]+.*$", "\\1", df$acceptedNameUsage), # mantém 3 palavras
    sub("^(([^ ]+ +){2})[^ ]*.*$", "\\1", df$acceptedNameUsage) # mantém 2 palavras
  )
  df$AcceptedName <- trimws(df$AcceptedName)

  # Ajustar valores quando NOT_FOUND
  df$AcceptedName[df$MatchType == "Not Found"] <- NA
  df$Genus[df$MatchType == "Not Found"] <- NA
  df$Genus[df$establishmentMeans == "Not Found"] <- NA
  df$Family <- df$family

  # Substituir informações de sinônimos por dados do nome aceito
  cor <- flora[
    flora$taxa %in% df$AcceptedName & flora$taxonomicStatus == "NOME_ACEITO",
  ]

  idx_sinonimos <- which(
    df$taxonomicStatus != "NOME_ACEITO" & !is.na(df$AcceptedName)
  )
  match_idx <- match(df$AcceptedName[idx_sinonimos], cor$taxa)
  validos <- which(!is.na(match_idx))

  cols_para_substituir <- c("establishmentMeans", "Endemism")
  cols_validas <- cols_para_substituir[
    cols_para_substituir %in% names(df) & cols_para_substituir %in% names(cor)
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
  # Selecionar e reordenar colunas finais
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
