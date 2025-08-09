#' @title Generate a Phytosociological Report
#'
#' @description This function generates a descriptive phytosociological report based on forest or herbaceous data.
#' It calculates metrics such as density, basal area, Shannon-Wiener diversity, and species importance values.
#' The report can be generated in Portuguese or English and optionally saved as a `.txt` file.
#'
#' WE ADVISE THAT THIS FUNCTION DON'T SUBSTITUTE A GOOD LOOK INTO YOUR DATA. WE ALSO ADVISE AGAINST THE RESULTS COPY AND PASTE.
#'
#' @param x A list containing two components:
#'   - `Phytossociology`: A data frame with phytosociological metrics. For `type = "forest"`, the columns must include
#'     `N` (density), `AB` (basal area), `FA` (frequency), `DoA` (dominance), and `VI` (importance value).
#'     For `type = "herb"`, the columns must include `FA` (frequency), `CA` (coverage), and `VI` (importance value).
#'   - `Resume`: A data frame with summarized metrics (e.g., total individuals, species richness, Shannon-Wiener diversity, etc.).
#' @param type Character. Type of vegetation. Use `"forest"` for tree data or `"herb"` for herbaceous data. Default is `"forest"`.
#' @param salvar_txt Logical. If `TRUE`, the generated report will be saved as a `.txt` file. Default is `FALSE`.
#' @param arquivo_txt Character. Name of the `.txt` file where the report will be saved. Default is `"relatorio_fitossociologico.txt"`.
#' @param lang Character. Language of the report. Use `"pt"` for Portuguese or `"us"` for English. Default is `"pt"`.
#'
#' @return A character string containing the phytosociological report.
#' If `salvar_txt = TRUE`, the report is also saved as a `.txt` file.
#'
#' @examples
#' data("Southern_Forest")
#' data("Southern_Camp")
#'
#' data <- phytos::PhytoIndex(Southern_Forest, 100)
#' data2 <- phytos::PhytoIndex(Southern_Camp, 100)
#'
#' # Generate a forest report in Portuguese
#' reportphyto(data, type = "forest", n = 3, salvar_txt = FALSE, lang = "pt")
#'
#' # Generate a herbaceous report in English and save to file
#' reportphyto(data2, type = "herb", salvar_txt = TRUE, arquivo_txt = "herb_report.txt", lang = "us")
#'
#' @export
reportphyto <- function(x, type = "forest", salvar_txt = FALSE, arquivo_txt = "relatorio_fitossociologico.txt", lang = "pt") {
  if (!lang %in% c("pt", "us")) {
    stop("lang. Use 'pt' or 'us'.")
  }

  if (type == "forest") {
    r <- round(x$Resume, 2)
    x <- x$Phytossociology
    colunas_necessarias <- c("N", "AB", "FA", "DoA", "VI")
    if (!all(colunas_necessarias %in% colnames(x))) {
      stop("A planilha deve conter as colunas: 'N', 'AB', 'FA', 'DoA', 'VI'.")
    }

    # Ordena os dados de acordo com as métricas
    VI <- x[order(-x$VI), ]
    N <- x[order(-x$N), ]
    AB <- x[order(-x$AB), ]
    FA <- x[order(-x$FA), ]
    DOA <- x[order(-x$DoA), ]

    # Obtém os nomes das espécies
    especie_VI <- row.names(VI[1:5, ])
    especie_N <- row.names(N[1:5, ])
    especie_AB <- row.names(AB[1:5, ])
    especie_FA <- row.names(FA[1:5, ])
    especie_DOA <- row.names(DOA[1:5, ])

    if (lang == "pt") {
      descricao <- paste0(
        sprintf(
          "Ao longo do estudo fitossociológico foram avaliados %s indivíduos, pertencentes a %s espécies botânicas. Observou-se uma densidade total de %s indivíduos por hectare e área basal de %s m²/ha. O nível de heterogeneidade na distribuição das espécies, junto com a riqueza observada, resultou em uma diversidade de Shannon-Wiener de %s nats e equidade de %s.",
          sum(x$N), r[3], r[1], r[2], r[4], r[5]
        ),
        sprintf(
          "\n\nA espécie %s foi a mais abundante dentro da comunidade, seguida por %s, %s, %s e %s.",
          especie_N[1], especie_N[2], especie_N[3], especie_N[4], especie_N[5]
        ),
        sprintf(
          "Em termos de frequência, %s e %s foram as espécies mais amplamente distribuídas entre as unidades amostrais, seguidas por %s, %s e %s.",
          especie_FA[1], especie_FA[2], especie_FA[3], especie_FA[4], especie_FA[5]
        ),
        sprintf(
          "\n\nNa dominância, %s destacou-se como a espécie com maior contribuição relativa para a biomassa total, junto com %s, %s, %s e %s.",
          especie_DOA[1], especie_DOA[2], especie_DOA[3], especie_DOA[4], especie_DOA[5]
        ),
        sprintf(
          "Por fim, considerando o Valor de Importância, que integra densidade, frequência e dominância, %s e %s ocuparam as posições de maior destaque, seguidas por %s, %s e %s.",
          especie_VI[1], especie_VI[2], especie_VI[3], especie_VI[4], especie_VI[5]
        )
      )
    } else if (lang == "us") {
      descricao <- paste0(
        sprintf(
          "Throughout the phytosociological study, %s individuals belonging to %s botanical species were evaluated. A total density of %s individuals per hectare and a basal area of %s m²/ha were observed. The heterogeneity level in species distribution, along with observed richness, resulted in a Shannon-Wiener diversity of %s nats and an evenness of %s.",
          sum(x$N), r[3], r[1], r[2], r[4], r[5]
        ),
        sprintf(
          "\n\nThe species %s was the most abundant within the community, followed by %s, %s, %s, and %s.",
          especie_N[1], especie_N[2], especie_N[3], especie_N[4], especie_N[5]
        ),
        sprintf(
          "In terms of frequency, %s and %s were the most widely distributed species among the sampling units, followed by %s, %s, and %s.",
          especie_FA[1], especie_FA[2], especie_FA[3], especie_FA[4], especie_FA[5]
        ),
        sprintf(
          "\n\nIn terms of dominance, %s stood out as the species with the highest relative contribution to the total biomass, along with %s, %s, %s, and %s.",
          especie_DOA[1], especie_DOA[2], especie_DOA[3], especie_DOA[4], especie_DOA[5]
        ),
        sprintf(
          "Finally, considering the Importance Value, which integrates density, frequency, and dominance, %s and %s occupied the highest positions, followed by %s, %s, and %s.",
          especie_VI[1], especie_VI[2], especie_VI[3], especie_VI[4], especie_VI[5]
        )
      )
    }
  }

  if (type == "herb") {
    r <- round(x$Resume, 2)
    x <- x$Phytossociology
    colunas_necessarias <- c("FA", "CA", "VI")
    if (!all(colunas_necessarias %in% colnames(x))) {
      stop("A planilha deve conter as colunas: 'FA', 'CA', 'VI'.")
    }

    # Ordena os dados de acordo com as métricas
    VI <- x[order(-x$VI), ]
    CA <- x[order(-x$CA), ]
    FA <- x[order(-x$FA), ]

    # Obtém os nomes das espécies
    especie_VI <- row.names(VI[1:5, ])
    especie_CA <- row.names(CA[1:5, ])
    especie_FA <- row.names(FA[1:5, ])

    if (lang == "pt") {
      descricao <- paste0(
        sprintf(
          "Ao longo do estudo fitossociológico foram levantadas %s espécies botânicas, com uma média de %s por unidade amostral. A diversidade de Shannon-Wiener foi de %s nats e a equidade foi de %s.",
          r[3, 2], r[4, 2], r[5, 2], r[6, 2]
        ),
        sprintf(
          "\n\nA espécie %s apresentou os maiores valores de cobertura, seguida por %s, %s, %s e %s.",
          especie_CA[1], especie_CA[2], especie_CA[3], especie_CA[4], especie_CA[5]
        ),
        sprintf(
          "Em termos de frequência, %s e %s foram as espécies mais amplamente distribuídas, seguidas por %s, %s e %s.",
          especie_FA[1], especie_FA[2], especie_FA[3], especie_FA[4], especie_FA[5]
        ),
        sprintf(
          "Por fim, considerando o Valor de Importância, que integra frequência e cobertura, %s e %s ocuparam as posições de maior destaque, seguidas por %s, %s e %s.",
          especie_VI[1], especie_VI[2], especie_VI[3], especie_VI[4], especie_VI[5]
        )
      )
    } else if (lang == "us") {
      descricao <- paste0(
        sprintf(
          "Throughout the phytosociological study, %s botanical species were surveyed, with an average of %s per sampling unit. The Shannon-Wiener diversity was %s nats and the evenness was %s.",
          r[3, 2], r[4, 2], r[5, 2], r[6, 2]
        ),
        sprintf(
          "\n\nThe species %s presented the highest coverage values, followed by %s, %s, %s, and %s.",
          especie_CA[1], especie_CA[2], especie_CA[3], especie_CA[4], especie_CA[5]
        ),
        sprintf(
          "In terms of frequency, %s and %s were the most widely distributed species, followed by %s, %s, and %s.",
          especie_FA[1], especie_FA[2], especie_FA[3], especie_FA[4], especie_FA[5]
        ),
        sprintf(
          "Finally, considering the Importance Value, which integrates frequency and coverage, %s and %s occupied the highest positions, followed by %s, %s, and %s.",
          especie_VI[1], especie_VI[2], especie_VI[3], especie_VI[4], especie_VI[5]
        )
      )
    }
  }

  # Salva o texto em um arquivo .txt, se solicitado
  if (salvar_txt) {
    writeLines(descricao, con = arquivo_txt)
    message("Relatório salvo em: ", arquivo_txt)
  }

  return(descricao)
}
