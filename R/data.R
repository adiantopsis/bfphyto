#' Forest Vegetation Dataset
#'
#' A dataset with sample data for forest vegetation, including plot ID, species, diameter at breast height, and height.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{parc}{Character. Plot ID.}
#'   \item{spp}{Character. Species name.}
#'   \item{dap}{Numeric. Diameter at breast height (cm).}
#'   \item{H}{Numeric. Height (m).}
#' }
#' @examples
#' data(Southern_Forest)
#' head(Southern_Forest)
"Southern_Forest"

#' Herbaceous Vegetation Dataset
#'
#' A dataset with sample data for herbaceous vegetation, including plot ID, species, coverage, and height.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{parc}{Character. Plot ID.}
#'   \item{spp}{Character. Species name.}
#'   \item{coverage}{Numeric. Coverage percentage.}
#'   \item{height}{Numeric. Height (m).}
#' }
#' @examples
#' data(Southern_Camp)
#' head(Southern_Camp)
"Southern_Camp"

#' @title Threatened Plant Species in Brazil (Portaria MMA nº 148/2022)
#'
#' @description
#' A dataset containing the list of plant species classified as threatened in Brazil,
#' according to the Brazilian Ministry of the Environment's ordinance Portaria MMA nº 148, published in 2022.
#' Please check the original document after use this list (https://www.in.gov.br/en/web/dou/-/portaria-mma-n-148-de-7-de-junho-de-2022-406272733).
#'
#' @format A data frame. Column names and structure depend on the original CSV.
#'
#' @source Ministério do Meio Ambiente (MMA), Portaria nº 148, de 7 de junho de 2022.
#'
#' @seealso \code{\link{ameacadas_rs}}
"ameacadas_br"

#' @title Threatened Brazilian Plants — CNCFlora 2020
#'
#' @description
#' A dataset listing threatened plant species in Brazil according to the CNCFlora (Centro Nacional de Conservação da Flora) 2020 assessment.
#' Please check the original source after use this list (https://cncflora.jbrj.gov.br/).
#'
#' @format A data frame. Column names and structure depend on the original CSV.
#'
#' @source CNCFlora (Centro Nacional de Conservação da Flora), 2020.
#'
#' @seealso \code{\link{ameacadas_br}}
"ameacadas_br_cnc2020"

#' @title Threatened Plant Species in Rio Grande do Sul (Decreto Estadual nº 52.109/2014)
#'
#' @description
#' A dataset listing plant species classified as threatened in the state of Rio Grande do Sul, Brazil,
#' based on the state decree Decreto nº 52.109, published in 2014.
#' Please check the original source after use this list (https://www.al.rs.gov.br/legis/M010/M0100099.ASP?Hid_Tipo=TEXTO&Hid_TodasNormas=61669&hTexto=&Hid_IDNorma=61669).
#'
#' @format A data frame. Column names and structure depend on the original CSV.
#'
#' @source Governo do Estado do Rio Grande do Sul, Decreto nº 52.109, de 1º de dezembro de 2014.
#'
#' @seealso \code{\link{ameacadas_br}}
"ameacadas_rs"

#' @title Rare Plant Species in Brazil
#'
#' @description
#' A dataset listing rare native plant species in Brazil. The criteria for rarity may include geographic distribution, endemism, and population size.
#'
#' @format A data frame. Column names and structure depend on the original CSV.
#'
#' @source Internal compilation from: Guillet et al. (2009) Plantas Raras do Brasil
"plantas_raras_br"
