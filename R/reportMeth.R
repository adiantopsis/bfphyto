#' Generate Methodology Report for Forest and Grassland Inventories
#'
#' This function automates the writing of the Methodology section for technical 
#' or scientific reports. It uses a parameter dataframe (typically imported from Excel) 
#' to calculate sampling efforts, convert inclusion criteria, and list software 
#' and package versions to ensure reproducibility.
#'
#' @param data A \code{data.frame} or \code{tibble} containing the columns 
#'   'Tipo' (Type), 'Parâmetro' (Parameter), and 'Valor' (Value). It must include 
#'   data for both 'Florestal' (Forest) and 'Campestre' (Grassland) strata.
#' @param pkg A character vector of package names whose versions should be 
#'   cited in the text. Defaults to \code{tidyverse}, \code{readxl}, 
#'   \code{ggplot2}, \code{bfphyto}, and \code{vegan}.
#'
#' @return A Markdown-formatted string containing the Floristic Survey, 
#'   Phytosociological Survey, and Data Analysis sections.
#' 
#' @importFrom glue glue
#' @importFrom dplyr filter
#' @importFrom purrr map_chr
#' @importFrom utils packageVersion
#' 
#' @examples
#' # Example usage with an imported dataframe
#' # df_met <- readxl::read_xlsx("methodology.xlsx")
#' # report_text <- reportMeth(df_met)
#' # cat(report_text)
#' @export

reportMeth <- function(data, pkg = c("tidyverse", "readxl", "ggplot2", "bfphyto", "vegan")) {
  
  # 1. Carregar Dados
  met <- data
  
  # Função interna para tratar números (converte vírgula para ponto se necessário)
  limpar_num <- function(x) as.numeric(gsub(",", ".", x))
  
  # 2. Captura de Versões do Sistema
  v_r <- R.Version()$version.string
  
  # CORREÇÃO AQUI: Alterado de 'pkgs' para 'pkg' para bater com o argumento da função
  v_pkgs <- purrr::map_chr(pkg, ~ as.character(utils::packageVersion(.x)))
  lista_pkgs <- paste(paste0(pkg, " (v", v_pkgs, ")"), collapse = ", ")
  
  # 3. Processamento Florestal
  f_df <- met %>% dplyr::filter(Tipo == "Florestal")
  f_n <- limpar_num(f_df$Valor[f_df$Parâmetro == "N"])
  f_tam <- limpar_num(f_df$Valor[f_df$Parâmetro == "Tamanho"])
  f_inc <- limpar_num(f_df$Valor[f_df$Parâmetro == "Inclusao"])
  f_esforco <- f_n * f_tam
  f_cap_equiv <- round(f_inc * pi, 1)
  
  # 4. Processamento Campestre
  c_df <- met %>% dplyr::filter(Tipo == "Campestre")
  c_n <- limpar_num(c_df$Valor[c_df$Parâmetro == "N"])
  c_tam <- limpar_num(c_df$Valor[c_df$Parâmetro == "Tamanho"])
  c_esforco <- c_n * c_tam
  
  # --- CONSTRUÇÃO DO TEXTO ---
  
  texto <- glue::glue("
## METODOLOGIA

### Levantamento Florístico 
O levantamento florístico envolveu caminhadas assistemáticas ao longo de pontos específicos da área de estudo. Durante o levantamento, deu-se ênfase a espécies exóticas, endêmicas, raras, ameaçadas de extinção ou protegidas por legislação vigente, incluindo a Portaria do MMA nº 148 de 2022 (Ministério do Meio Ambiente 2022), Decreto 52.109 de 2014 (Rio Grande do Sul, 2014),  Lista Vermelha da IUCN (IUCN 2025).

Para identificar os espécimes observados foram feitas coletas de materiais frescos, essa realizada apenas quando não foi possível identificá-los em campo. A análise desse material foi realizada posteriormente a partir de consultas em literatura especializada. Os mesmos procedimentos listados anteriormente também foram adotados para atualização da grafia e atualizações nomenclaturais, determinação de espécies raras e ameaçadas no banco de dados, identificação quanto a origem (nativo vs. exótico) e nível de endemismo. Para isso utilizou-se a função resolve_spp do pacote bfphyto (v.0.0.1) (https://github.com/adiantopsis/bfphyto).

### Levantamento fitossociológico

Para o componente **Florestal**, foram utilizadas unidades amostrais retangulares de {f_df$Valor[f_df$Parâmetro == 'UA']} metros ({f_tam} ha), distribuídas via {f_df$Valor[f_df$Parâmetro == 'Sistema']}. Ao total, foram implantadas {f_n} parcelas, totalizando um esforço amostral de {f_esforco} hectares. O critério de inclusão adotado foi de diâmetro à altura do peito (DAP) ≥ {f_inc} cm, o que equivale a uma circunferência (CAP) ≥ {f_cap_equiv} cm.

A partir dos dados coletados foram calculados os descritores fitossociológicos para caracterização da estrutura horizontal da vegetação: densidade absoluta (DA), densidade relativa (DR), área basal (AB), frequência absoluta (FA), frequência relativa (FR), dominância absoluta (DoA), dominância relativa (DoR) e valor de importância (IV) (Dieter Mueller-Dombois & Heinz Ellenberg 1974). 

Para a vegetação **Campestre**, adotou-se o sistema de amostragem {c_df$Valor[c_df$Parâmetro == 'Sistema']}. Foram alocadas {c_n} unidades de {c_df$Valor[c_df$Parâmetro == 'UA']} metros ({c_tam} m²), resultando em um esforço de {c_esforco} m². {c_df$Valor[c_df$Parâmetro == 'Descrição']}

Os descritores fitossociológicos considerados para os campos e a vegetação de praia: frequência absoluta (FA), frequência relativa (FR), cobertura absoluta (CoA), cobertura relativa (CR) e valor de importância (IV) (Dieter Mueller-Dombois & Heinz Ellenberg 1974). O valor de importância das espécies vegetais foi obtido através da soma da frequência e cobertura relativa, dividido por dois. A diversidade de espécies foi calculada através do índice de Shannon (H’), utilizando o logaritmo natural (nats) e dados de cobertura absoluta como uma aproximação da abundância das espécies (Magurran 2004).


### Análise de Dados

A análise de dados foi executada integralmente em ambiente R, com auxílio do software {v_r} (RStudio Team 2025). A suficiência amostral foi testada através de uma curva de acumulação de riqueza (curva do coletor), na qual a amostragem foi considerada suficiente quando um aumento de 10% no esforço amostral correspondeu a um incremento menor que 10% no número de espécies levantadas (Cain 1943). A curva do coletor foi elaborada com auxílio da função specaccum do pacote vegan (Oksanen et al. 2022).

O cálculo dos descritores fitossociológicos de estrutura horizontal também foi realizado no ambiente estatístico R com auxílio do pacote bfphyto (Machado 2024).  Para comparação das fisionomias em termos de riqueza, diversidade e composição taxonômica foram utilizadas funções específicas do pacote vegan (Oksanen et al. 2022). As posteriores análises dos dados foram feitas utilizando-se as funções da base do software R, em conjunto com os pacotes: {lista_pkgs} e vegan.

As coordenadas geográficas das unidades amostrais e espécies de interesse especial para conservação foram registradas via SW Maps.
")
  
  return(texto)
}
