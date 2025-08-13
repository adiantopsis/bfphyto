#' Function for calculating phytosociological descriptors and adjusting phytosociological data tables
#'
#' This function calculates phytosociological descriptors and performs adjustments on data tables.
#' It is largely adapted from the `PhytoIndex` function available at https://github.com/ricds/fitoR, created by
#' Alexandre Gabriel Christo, Pedro Higuchi, Ricardo Dal'Agnol, and Arthur Vinicius Rodrigues.
#'
#' @details
#' The input file must contain the following columns with exact lowercase names:
#' - `parc`: plot identification (numeric or character, either works)
#' - `spp`: common or scientific name of the individual
#' - `dap`: diameter at breast height (DBH) in cm
#' or
#' - `cap`: circumference at breast height (CBH) in cm
#'
#' It is advisable to use separate columns for individuals with multi-stemmed trunks, named as: `cap` or `dap1`, `dap2`, `dap3`, `dap4`, etc. Alternatively, use the equivalent diameter. Do NOT separate the stems of individuals into rows.
#'
#' Note: The script will search for the keywords `cap` and `dap` in column names to locate them.
#' Therefore, avoid having extra columns containing these keywords in their names (e.g., a column named `dap_medio` could cause calculation errors).
#'
#' @title treePhyto
#' @param x A data frame containing the columns: plots (`parc`), species (`spp`), height (`H`), and either diameters (`dap`) or circumferences (`cap`).
#' @param area The area of the sampling units in square meters.
#' @param VI The type of importance value. Options are `"cottam"` for the sum of relativized parameters, or `"percent"` (default) for the mean of relativized parameters.
#' @param filename The name of the output file for the results, provided as a string (single or double quotes), default is NULL.
#' @importFrom stats sd
#' @return A list summarizing the data, including:
#' - Basal area (m²/ha)
#' - Density (ind./ha)
#' - Richness (excluding dead specimens from the raw data)
#' - Shannon-Wiener diversity index (in nats)
#' - Pielou's evenness index
#' @examples
#' data(Southern_Forest)
#' treePhyto(x = Southern_Forest, area = 100, VI = "cottam", filename = "my_result")
#' @export
treePhyto <- function(x, area, filename = NULL, VI = "percent") {
  if (missing(VI)) {
    VI <- "cottam"
  } else {
    VI <- VI
  }
  matriz <- table(x$spp, x$parc)

  # numero de parcelas
  nparc <- length(levels(as.factor(x$parc)))

  # area total amostrada
  area.parc <- (area * nparc)

  # densidade
  dta <- length(x$spp) / (area.parc / 10000)

  # desvio da densidade entre parcelas
  dtadesv <- 0
  dtai <- 1
  vetor <- 1
  while (dtai <= nparc) {
    length(vetor) <- nparc
    vetor[dtai] <- sum(matriz[, dtai])
    dtadesv <- sd(vetor) / (area / 10000)
    dtai <- dtai + 1
  }

  # calcula o numero de ind amostrados
  N <- apply(matriz, 1, sum)

  # calcula densidades
  DA <- apply(matriz, 1, sum) / (area.parc / 10000)
  DR <- DA / sum(DA) * 100

  # calcula frequencias
  freq <- (if (length(dim(matriz)) > 1) {
    apply(matriz > 0, 1, sum)
  } else {
    sum(matriz > 0)
  })
  FA <- (freq / nparc) * 100
  FR <- (FA / sum(FA)) * 100

  # checa por NAs nos dados e transforma em zeros
  x[is.na(x)] <- 0

  # determina se existe "caps" ou "daps" e quais colunas estão
  cols <- grep("cap", colnames(x))
  ncols <- length(cols)
  if (ncols > 0) param <- "cap"

  cols2 <- grep("dap", colnames(x))
  ncols2 <- length(cols2)
  if (ncols2 > 0) param <- "dap"

  if (param == "dap") cols <- cols2
  if (param == "dap") ncols <- ncols2

  # calcula a area da seção transversal para cada cap/dap e faz a soma por individuo
  i <- 1
  x$areasec <- 0
  while (i <= ncols) {
    if (param == "cap") x$areasec <- x$areasec + ((pi * (x[, cols[i]] / pi)^2) / 40000)
    if (param == "dap") x$areasec <- x$areasec + ((pi * x[, cols[i]]^2) / 40000)
    i <- i + 1
  }

  # calcula as dominancias
  DoA <- tapply(x$areasec, x$spp, sum) / (area.parc / 10000)
  DoR <- DoA / sum(DoA) * 100

  # area basal por espécie
  AB <- tapply(x$areasec, x$spp, sum)

  # area basal
  abta <- sum(DoA)

  # desvio da area basal entre parcelas
  somag <- tapply(x$areasec, x$parc, sum) / (area / 10000)
  abdesv <- sd(somag)

  # calcula o indice de valor de importancia
  if (VI == "cottam") {
    VI <- (DR + DoR + FR)
  } else {
    if (VI == "percent") {
      VI <- (DR + DoR + FR) / 3
    }
  }

  # monta a tabela
  fito <- data.frame(
    N = N,
    AB = AB,
    DA = DA,
    DR = DR,
    DoA = DoA,
    DoR = DoR,
    FA = FA,
    FR = FR,
    VI = VI
  )

  fito <- fito[order(VI, decreasing = TRUE), ]

  # calcula os indices de diversidade
  Pi <- N / sum(N)
  Pi <- Pi * log(Pi)
  SW <- -sum(Pi)
  S <- nrow(fito) - 1
  J <- SW / log(S)

  p <- rbind(
    "Total density" = dta,
    "Basal area" = round(abta, digits = 2),
    Richness = S,
    Diversity = SW,
    Eveness = J
  )

  l <- list(
    Resume = p,
    Phytossociology = fito
  )

  return(l)
}
