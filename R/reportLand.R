#' Relatório de Paisagem e Biomas (IBGE)
#'
#' Esta função realiza uma análise espacial baseada em coordenadas geográficas para identificar
#' o Estado e o Bioma (focando em Mata Atlântica e Pampa). Ela gera automaticamente um texto
#' técnico referenciado (IBGE/Veloso) e um mapa profissional de localização.
#'
#' @param lat Numérico. Latitude em graus decimais (Datum WGS84).
#' @param lon Numérico. Longitude em graus decimais (Datum WGS84).
#'
#' @return Uma lista (\code{list}) contendo dois elementos:
#' \itemize{
#'   \item \code{texto}: Uma string formatada em Markdown/Glue com a caracterização fitogeográfica.
#'   \item \code{mapa}: Um objeto \code{ggplot} contendo o mapa de localização com escala e norte.
#' }
#'
#' @details
#' A função utiliza os dados oficiais do IBGE disponibilizados pelo pacote \code{geobr}.
#' Os textos descritivos baseiam-se em Veloso et al. (1991) e nos Manuais Técnicos da
#' Vegetação Brasileira. Atualmente, descrições detalhadas estão disponíveis apenas para
#' os biomas "Mata Atlântica" e "Pampa".
#'
#' @importFrom geobr read_state read_biomes
#' @importFrom sf st_point st_sfc st_sf st_transform st_intersects st_intersection st_join
#' @importFrom ggplot2 ggplot geom_sf aes scale_fill_manual labs theme_minimal theme
#' @importFrom ggspatial annotation_north_arrow annotation_scale north_arrow_fancy_orienteering
#' @importFrom dplyr %>%
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' # Exemplo para uma coordenada no RS (Pampa)
#' report <- reportLand(lat = -30.0346, lon = -51.2177)
#' cat(report$texto)
#' print(report$mapa)
#' }
#' @export
reportLand <- function(lat,
                       lon,
                       year = 2019,
                       shape = 8,
                       caption = "Fonte: IBGE (2018)\n Datum: SIRGAS2000" ){
        library(geobr)
        library(sf)
        library(ggplot2)
        library(ggspatial)
        library(dplyr)
        library(glue)
        # 1. Obter dados espaciais (IBGE)
        ponto <- st_point(c(lon, lat)) %>%
                st_sfc(crs = 4326) %>%
                st_sf()
        estados <- read_state(year = year, showProgress = TRUE)
        biomas <- read_biomes(year = year, showProgress = TRUE)

        estado_local <- estados[
                st_intersects(
                        ponto,
                        st_transform(estados, crs = 4326),
                        sparse = FALSE
                ),
        ]
        bioma_local <- biomas[
                st_intersects(
                        ponto,
                        st_transform(biomas, crs = 4326),
                        sparse = FALSE
                ),
        ]
        nome_bioma <- bioma_local$name_biome

        textos_biomas <- list(
                "Mata Atlântica" = glue(
                        "
      O ponto amostral está inserido no domínio da **Mata Atlântica**, um ecossistema caracterizado por alta complexidade estrutural e riqueza de endemismos. Segundo a classificação do IBGE e Veloso et al. (1991), este bioma engloba formações de Floresta Ombrófila Densa, Mista e Estacional Semidecidual. A fitofisionomia da região é moldada por regimes pluviométricos elevados e constantes, resultando em uma vegetação com dossel fechado, estratificação vertical definida e abundância de epífitas e lianas. O estado de {estado_local$name_state} abriga remanescentes vitais para a conectividade da paisagem e manutenção dos serviços ecossistêmicos hídricos."
                ),
                "Pampa" = glue(
                        "
O bioma Pampa, que abrange cerca de 2% do território brasileiro no extremo sul do país, possui uma história evolutiva profunda, com registros de sua vegetação herbácea remontando ao Mioceno Médio e tornando-se dominante há cerca de 7 milhões de anos. Durante o Plioceno e o Pleistocene, a região foi dominada por campos em um cenário de clima mais frio e seco, sendo que o Pampa atual é interpretado como um relicto da variação climática passada e da presença de grandes herbívoros, uma vez que as condições climáticas presentes já seriam adequadas para o desenvolvimento de florestas (Della & Prado, 2025). A composição florística desse bioma foi moldada por eventos de dispersão assimétricos vindos principalmente da Mata Atlântica, dos Andes, do Cerrado e do Chaco, ocorridos majoritariamente nos últimos 5 milhões de anos (Della & Prado, 2025).

A riqueza biológica do Pampa é expressiva, com estimativas que variam entre 3.000 e 4.000 espécies de plantas vasculares, apresentando um índice de endemismo em torno de 8% (Fiaschi & Pirani, 2009; Della & Prado, 2025). A flora é composta por uma mistura de elementos megatérmicos e microtérmicos, destacando-se como famílias botânicas características as Poaceae, Asteraceae, Cyperaceae, Fabaceae, Apiaceae, Oxalidaceae, Verbenaceae e Iridaceae (Della & Prado, 2025). Estruturalmente, a paisagem é dominada por uma matriz campestre de ervas e arbustos, embora inclua também savanas, estepes e matas ciliares, organizando-se em quatro grandes conjuntos vegetacionais: o Planalto da Campanha, a Depressão Central, o Planalto Sul-Rio-Grandense e a Planície Costeira. O clima é chuvoso e sem período seco, mas marcado por temperaturas negativas e geadas no inverno que exercem forte controle sobre a vegetação (IBGE, 2019).

A importância do Pampa reside em sua biodiversidade única adaptada a extremos térmicos e em sua relevância socioeconômica, sendo historicamente utilizado como pastagem natural para a pecuária, além de abrigar grandes áreas de cultivo agrícola, com destaque para a produção de arroz (IBGE, 2019). Apesar de sua resiliência histórica, ele é reconhecido como um bioma frágil e frequentemente negligenciado, enfrentando desafios constantes para a manutenção de seus serviços ecossistêmicos diante da uniformização da cobertura vegetal causada pela interferência humana (IBGE,2019; Della & Prado, 2025).
"
                )
        )

        # Seleção do texto
        descricao_especifica <- textos_biomas[[nome_bioma]]

        if (is.null(descricao_especifica)) {
                descricao_especifica <- "A coordenada está fora dos biomas Pampa ou Mata Atlântica. Verifique os dados espaciais."
        }

        # 3. Construção do Report
        report_texto <- glue(
                "
  ### Caracterização Fitogeográfica: {nome_bioma}

  A área de estudo situa-se no estado de {estado_local$name_state} ({estado_local$abbrev_state}), com centroide localizado na coordenada decimal Lat {lat} / Lon {lon}.

  {descricao_especifica}

  A espacialização do ponto sobre as malhas digitais do IBGE confirma a inserção nos limites biogeográficos oficiais do bioma {nome_bioma}, o que é essencial para a interpretação dos aspectos legais associados à flora envolvidos no procedimento de licenciamento ambiental.
  "
        )

        mapa <-
                ggplot() +
                geom_sf(
                        data = st_intersection(biomas, estado_local),
                        aes(fill = name_biome),
                        color = "grey",
                        size = 0.1,
                        alpha = 0.8
                ) +
                geom_sf(
                        data = estado_local,
                        fill = NA,
                        color = "black",
                        size = 0.6
                ) +
                geom_sf(
                        data = ponto,
                        aes(col = "Área de estudo"),
                        size = 3,
                        shape = shape
                ) +
                scale_fill_brewer(palette = "Greens", direction = -1) +
                annotation_north_arrow(
                        location = "bl",
                        which_north = "true",
                        style = north_arrow_nautical()
                ) +
                annotation_scale(
                        location = "br",
                        width_hint = 0.1,
                        text_cex = .5
                ) +
                labs(
                        subtitle = paste(estado_local$name_state),
                        x = " ",
                        y = " ",
                        colour = " ",
                        fill = " ",
                        caption = caption
                ) +
                theme_bw() +
                theme(
                        legend.position = "bottom",
                        panel.background = element_rect(
                                fill = "#f0f8ff",
                                color = NA
                        )
                )
        mapa
        return(list(texto = report_texto, mapa = mapa))
}
