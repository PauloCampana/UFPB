#' @title Mapas do Brasil
#' @description Mapas do Brasil com polígonos contendo informação sobre Região, Estado ou Município que o polígono faz parte.
#'
#' @format Uma tibble com 4 colunas e número variado de linhas:
#' \describe{
#'   \item{long, lat}{Longitude e Latitude}
#'   \item{group}{Caracteriza o polígono}
#'   \item{id}{ID da Região, Estado ou Município}
#' }
#'
#' @details Os mapas possuem qualidades diferentes, máxima para Região e Estado, qualidade mínima para Município.
#' @source <https://servicodados.ibge.gov.br/api/docs/malhas>
#' @name mapa
#' @keywords datasets
NULL

#' @rdname mapa
"mapa_regiao"
#' @rdname mapa
"mapa_estado"
#' @rdname mapa
"mapa_municipio"
