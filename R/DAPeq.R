#' Function DAPeq
#'
#' Calculates the equivalent diameter for multi-stemmed plants.
#'
#' @title DAPeq
#' @param x A data frame containing the columns: plots (`parc`), species (`spp`), height (`H`), and either diameters (`dap`) or circumferences (`cap`).
#' @param ff The form factor to use in calculations. Defaults to `0.5` if not provided.
#' @return A new data frame with the equivalent diameter and plant volumes, calculated based on the specified form factor.
#' @details The equivalent diameter is calculated for plants with multi-stemmed trunks. The results include the volume of each plant, considering the provided or default form factor.
#' @examples
#' data(Southern_Forest)
#' dap.eq(x = Southern_Forest, ff = 0.5)
#' @export
DAPeq <- function(x, ff) {
  df <- x
  if (missing(ff)) {
    ff <- 0.5
  } else {
    ff <- ff
  }
  if (grep("cap|CAP", colnames(df))) {
    cap <- grep("cap|CAP", colnames(df))
    df[, cap] <- df[, cap] / pi
    colnames(df)[cap] <- paste0(rep("DAP"), seq_along(cap))
  }
  DAP <- grep("dap|DAP", colnames(df))
  if (length(DAP) > 1) {
    if (df[is.na(df)]) {
      df[is.na(df)] <- 0
    }
    col_dap <- grep("dap|DAP", colnames(df))
    df$dap_eq <- sqrt(rowSums(df[, col_dap]^2))
    g <- ((pi * (df$dap_eq^2)) / 40000)
    g1 <- g * df$H * ff
    df$vm3 <- g1
    return(df[, -col_dap])
  } else {
    df <- df
    g <- ((pi * (df$dap_eq^2)) / 40000)
    g1 <- g * df[, grep("H", colnames(df))] * ff
    df$vm3 <- g1
    return(df[, -col_dap])
  }
}
