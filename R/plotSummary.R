#' Function to summarize forest inventory data by sampling unit
#'
#' This function summarizes forest inventory data, requiring the following packages: `reshape`, `tidyverse`, and `vegan`.
#'
#' @details
#' The input file must contain the following columns with exact lowercase names:
#' - `parc`: plot identification (numeric or character, either works)
#' - `spp`: common or scientific name of the individual
#' - `dap`: diameter at breast height (DBH) in cm
#' or
#' - `cap`: circumference at breast height (CBH) in cm
#' - `H`: height in meters
#'
#' It is advisable to use separate columns for individuals with multi-stemmed trunks, named as: `cap` or `dap1`, `dap2`, `dap3`, `dap4`, etc. Alternatively, use the equivalent diameter. Do NOT separate the stems of individuals into rows.
#'
#' Note: The script will search for the keywords `cap` and `dap` in column names to locate them. Therefore, avoid having extra columns containing these keywords in their names (e.g., a column named `dap_medio` could cause calculation errors).
#' @importFrom stats sd
#' @title parc_resume
#' @param data A data frame containing the columns: plots (`parc`), species (`spp`), height (`H`), and either diameters (`dap`) or circumferences (`cap`).
#' @return A list summarizing the data for each sampling unit, including:
#' - Abundance
#' - Richness (removing dead specimens is recommended to avoid counting them here)
#' - Shannon-Wiener diversity index (in nats)
#' - Mean and standard deviation of height (in meters)
#' - Mean and standard deviation of DBH or CBH (in cm)
#' - Pielou's evenness index
#' - Basal area (in m²)
#' @examples
#' data(Southern_Forest)
#' plotSummary(data = Southern_Forest)
#' @export

plotSummary <- function(data) {
  cols = grep('cap', colnames(data))
  ncols = length(cols)
  if (ncols > 0) param = "cap"

  cols2 = grep('dap', colnames(data))
  ncols2 = length(cols2)
  if (ncols2 > 0) param = "dap"

  if (param == "dap") cols = cols2
  if (param == "dap") ncols = ncols2

  if (param == "cap") {
    data[, cols] <- data[, cols] / 3.14
    colnames(data)[cols] <- paste0(rep("dap", ncols), seq(1, ncols, 1))
  } else {
    data <- data
  }

  #Riqueza e abundancia
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
  data_clean <- data[!data$spp == "Morta", ]
  data_raw <- data
  ab <- table(data_clean$parc, data_clean$spp)

  freq <- ab
  freq[freq > 0] <- 1

  S <- apply(freq, 1, sum) #Riqueza por UA

  #Diversidade e equitatividade
  Pi <- ab / rowSums(ab)
  Pi <- Pi * log(Pi)
  SW = -rowSums(Pi, na.rm = TRUE)
  S <- rowSums(ifelse(ab > 0, 1, 0))
  J = SW / log(S)

  #Diametro
  N_raw <- table(data_raw$parc, data_raw$spp)
  N <- rowSums(N_raw)
  df <- data.frame(data_raw)
  df[is.na(df)] <- 0
  DAP <- grep("dap", colnames(df))
  len <- ifelse(df[, DAP] > 0, 1, 0)
  l <- apply(len, 1, sum)
  q <- which(l > 1)
  df[q, DAP] <- df[q, DAP]^2
  df$dap_eq <- df[, DAP[1]]
  df$dap_eq[q] <- sqrt(apply(df[q, DAP], 1, sum))

  df <- cbind.data.frame(
    parc = df$parc,
    spp = df$spp,
    H = df$H,
    dap_eq = df$dap_eq
  )
  par_sum <- tapply(df$dap_eq, df$parc, sum)
  mean_DAP <- tapply(df$dap_eq, df$parc, mean)
  sd_DAP <- tapply(df$dap_eq, df$parc, sd)

  #Area basal
  df$g <- (pi * df$dap_eq^2) / 40000
  AB <- tapply(df$g, df$parc, sum)

  #Altura

  mean_H <- tapply(data_raw$H, data_raw$parc, mean)

  sd_H <- tapply(data_raw$H, data_raw$parc, sd)

  #Data Frame final
  Parc_res = cbind.data.frame(
    N,
    Richness = S,
    Shannon = SW,
    Pielow = J,
    mean_H,
    sd_H,
    mean_DAP,
    sd_DAP,
    AB
  )
  print(Parc_res)
  return(list(Resume = Parc_res, Input = df))
}
