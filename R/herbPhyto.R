#' The `plantCover` function calculates phytosociological parameters for herbaceous vegetation based on cover values.
#'
#' The input file must include the following columns with exact lowercase names:
#' - `parc`: plot identification (numeric or character, either works)
#' - `spp`: common or scientific name of the individual
#' - `cob`: absolute cover percentage
#'
#' @title plant_cover
#' @param x A data frame containing the columns: plots (`parc`), species (`spp`), and absolute cover (`cob`).
#' @param area The area of the sampling units in square meters.
#' @return A list summarizing the data, including the number of sampling units, total sampled area, species richness, Shannon-Wiener diversity index (in nats, based on cover), and Pielou's evenness index.
#' @examples
#' Example usage:
#' data(Southern_Camp)
#' herbPhyto(x = Southern_Camp, area = 1)
#' @export
herbPhyto <- function(x, area) {
  matriz <- table(x$spp, x$parc)

  #numero de parcelas
  nparc <- length(levels(as.factor(x$parc)))

  #area total amostrada
  area.parc = (area * nparc)

  #calcula frequencias
  freq <- (if (length(dim(matriz)) > 1) {
    apply(matriz > 0, 1, sum)
  } else sum(matriz > 0))
  FA <- (freq / nparc) * 100
  FR <- (FA / sum(FA)) * 100

  #Cobertura

  CA <- tapply(x$cob, x$spp, sum)
  CR <- (CA / sum(CA)) * 100

  #calcula o indice de valor de importancia
  VI <- (CR + FR) / 2

  #monta a tabela
  fito = data.frame(FA = FA, FR = FR, CA, CR, VI = VI)
  fito$FR <- round(fito$FR, digits = 4)
  fito$FA <- round(fito$FA, digits = 4)
  fito$CA <- round(fito$CA, digits = 4)
  fito$CR <- round(fito$CR, digits = 4)
  fito$VI <- round(fito$VI, digits = 4)
  fito <- fito[order(VI, decreasing = TRUE), ]

  to_remove <- c(
    'rocha',
    'mantilho',
    'morta',
    'areia',
    'solo exposto',
    'Rocha',
    'Mantilho',
    'Morta',
    'Areia',
    'Solo exposto',
    'rock',
    'litter',
    'dead',
    'sand',
    'bare soil',
    'Rock',
    'Litter',
    'Dead',
    'Sand',
    'Bare soil'
  )
  #calcula os indices de diversidade
  div <- fito[!row.names(fito) %in% to_remove, ]
  Pi <- div$CA / sum(div$CA)
  Pi <- Pi * log(Pi)
  SW = -sum(Pi)
  S <- nrow(div)
  J = SW / log(S)

  w <- x[!x$spp %in% to_remove, ]
  parc_green_cob <- tapply(w$cob, w$parc, sum)

  n_green <- x[x$spp %in% to_remove, ]

  parc_n_green <- tapply(n_green$cob, n_green$parc, sum)

  parc_spp <- tapply(w$spp, w$parc, length)

  parc_div <- tapply(w$cob, w$parc, FUN = function(x) {
    p_i <- x / sum(x)
    # Calcula o índice de Shannon
    -sum(p_i * log(p_i), na.rm = TRUE)
  })
  sum_parc = cbind(
    richness = parc_spp,
    diversity = parc_div,
    gree_cover = parc_green_cob
  )

  mode_spp <- names(sort(-table(parc_spp)))[1]
  mode_cob <- names(sort(-table(parc_green_cob)))[1]
  mean_spp <- mean(parc_spp)
  mean_cob <- mean(parc_green_cob)

  resume = cbind.data.frame(
    Parâmetros = c(
      "Plots",
      "Sampled area",
      'Richness',
      'Plot richness mode',
      'Plot richness mean',
      "Mean plot green cover",
      "Shannon-Wiener (H') diversity",
      "Pielou (J) eveness"
    ),
    Resultado = c(
      round(nparc, digits = 2),
      round(area.parc, digits = 2),
      S,
      mode_spp,
      mean_spp,
      mean_cob,
      round(SW, digits = 2),
      round(J, digits = 2)
    )
  )
  l <- list(fitossociologia = fito, resumo_ua = sum_parc, resumo_geral = resume)
  return(l)
}
