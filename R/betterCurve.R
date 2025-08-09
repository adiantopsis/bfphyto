#' Generate species accumulation curves for phytosociological data
#'
#' This function computes and plots species accumulation curves based on phytosociological data,
#' using different accumulation methods: \code{"collector"}, \code{"exact"}, \code{"random"}, and \code{"inext"}.
#' It relies on the packages \pkg{vegan}, \pkg{iNEXT}, \pkg{reshape2}, \pkg{dplyr}, and \pkg{ggplot2}.
#'
#' @param x Data frame containing phytosociological data in long format, with mandatory columns \code{parc} (plot), \code{spp} (species), and the variable specified in \code{value}.
#' @param plot_area Numeric. Area (m²) of each plot or sampling unit, used to calculate accumulated area.
#' @param method Character. Method for calculating the accumulation curve. Must be one of: \code{"collector"}, \code{"exact"}, \code{"random"}, or \code{"inext"}.
#' @param value Character. Variable used for calculation: \code{"cob"} (coverage) or another variable indicating occurrence counts.
#' @param colour Character. Color used in the plot for the ribbon and points of the curve (e.g., \code{"lightgreen"}).
#' @param q Numeric. Order of Hill number for the \code{"inext"} method. Default is \code{0}, corresponding to species richness.
#' @param lang Character. Language for axis labels and legends. Can be \code{"pt"} (Portuguese) or \code{"us"} (English). Default is \code{"pt"}.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{plot}{A \code{ggplot} object with the species accumulation curve plot.}
#'   \item{result}{An object with the calculation results of the selected method (\code{iNEXT} or \code{specaccum}).}
#' }
#'
#' @details
#' For the \code{"inext"} method, the function performs rarefaction and extrapolation estimates using \code{iNEXT::iNEXT} and plots them with \code{iNEXT::ggiNEXT}.
#' For the \code{"collector"}, \code{"exact"}, and \code{"random"} methods, it uses \code{vegan::specaccum} to compute accumulation curves and plots with \pkg{ggplot2}.
#'
#' The function transforms data into a presence/absence matrix before calculations.
#'
#' @examples
#' \dontrun{
#' # Assuming 'data' is a data.frame with columns 'parc', 'spp', and 'cob'
#' betterCurve(data, plot_area = 100, method = "collector", value = "cob", colour = "blue", lang = "us")
#'
#' betterCurve(data, plot_area = 100, method = "inext", q = 0, lang = "us")
#' }
#'
#' @importFrom ggplot2 ggplot geom_ribbon geom_line geom_point theme_bw labs scale_linetype_manual theme element_text
#' @importFrom reshape2 acast
#' @importFrom dplyr %>%
#' @importFrom iNEXT iNEXT ggiNEXT
#' @importFrom vegan specaccum
#'
#' @export

betterCurve <- function(x,
                        plot_area,
                        method = "collector",
                        value = "cob",
                        colour = "lightgreen",
                        q = 0,
                        lang = "pt") {
  # Pacotes usados
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required")
  if (!requireNamespace("iNEXT", quietly = TRUE)) stop("Package 'iNEXT' is required")
  if (!requireNamespace("vegan", quietly = TRUE)) stop("Package 'vegan' is required for some methods")
  if (!requireNamespace("reshape2", quietly = TRUE)) stop("Package 'reshape2' is required")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required")

  library(dplyr)
  library(reshape2)
  library(ggplot2)
  library(iNEXT)
  library(vegan)

  # Labels conforme linguagem
  if (lang == "us") {
    xlab <- "Area (m²)"
    ylab <- "Richness"
    line_inext <- c("Rarefaction", "Extrapolation")
  } else if (lang == "pt") {
    xlab <- "Área (m²)"
    ylab <- "Riqueza"
    line_inext <- c("Rarefação", "Extrapolação")
  } else {
    stop("Unsupported language. Use 'pt' or 'us'.")
  }

  # Monta matriz com reshape2
  if (value == "cob") {
    col <- reshape2::acast(x, parc ~ spp, value.var = value, sum)
  } else {
    col <- reshape2::acast(x, parc ~ spp, value.var = value, length)
  }

  # Presença/ausência
  w_pa <- ifelse(col == 0, 0, 1)

  # Vetor área acumulada
  data3 <- data.frame(Sites = seq(from = plot_area, to = nrow(w_pa) * plot_area, by = plot_area))

  # Método inext
  if (method == "inext") {
    col1 <- apply(w_pa, 2, sum) %>%
      append(values = c(Total = nrow(col))) %>%
      sort(decreasing = TRUE)

    out1 <- iNEXT(x = col1, q = q, datatype = "incidence_freq")

    p <- iNEXT::ggiNEXT(out1, type = 1, color.var = "Order.q") +
      theme_bw() +
      labs(x = xlab, y = ylab) +
      scale_linetype_manual(values = c(1, 3), labels = line_inext)

    return(list(result = out1, plot = p))
  }

  # Métodos com vegan::specaccum (random, exact e collector)
  if (method %in% c("random", "exact")) {
    r <- vegan::specaccum(w_pa, method = method, permutations = ifelse(method == "random", 999, 0))

    data1 <- data.frame(Sites = data3$Sites, Richness = r$richness, SD = r$sd)

    b <- ggplot(data1) +
      geom_ribbon(aes(x = Sites, ymin = Richness - SD, ymax = Richness + SD), fill = colour, alpha = 0.5) +
      geom_line(aes(x = Sites, y = Richness), color = "black", linetype = 6) +
      geom_point(aes(x = Sites, y = Richness), size = 4, shape = 21, fill = colour, color = colour) +
      theme_bw(base_family = "sans", base_size = 12) +
      theme(
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        axis.title = element_text(family = "sans", face = "bold", colour = "black"),
        axis.text = element_text(colour = "black")
      ) +
      labs(x = xlab, y = ylab)

    return(list(plot = b, result = r))
  }

  if (method == "collector") {
    c <- vegan::specaccum(w_pa, method = "collector")
    data2 <- data.frame(Sites = data3$Sites, Richness = c$richness)

    a <- ggplot(data2) +
      geom_line(aes(x = Sites, y = Richness), color = "black", linetype = 6) +
      geom_point(aes(x = Sites, y = Richness), size = 4, shape = 21, fill = colour, color = colour) +
      theme_bw(base_family = "sans", base_size = 12) +
      theme(
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        axis.title = element_text(family = "sans", face = "bold", colour = "black"),
        axis.text = element_text(colour = "black")
      ) +
      labs(x = xlab, y = ylab)

    return(list(plot = a, result = c))
  }

  stop("Method not found. Please choose one of: 'inext', 'collector', 'exact', or 'random'.")
}
