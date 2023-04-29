#' @title Obter URL do mapa
#' @description Gera link do serviço de dados do IBGE para poder baixar arquivo GEOJSON contendo uma malha geográfica.
#'
#' @param nivel     Escopo do mapa, um de ("pais", "regiao", "estado", "mesoregiao", "microregiao", "regiaointermediaria", "regiaoimediata", "municipio").
#' @param id        ID do Escopo, 1 digito para "regiao", 2 para "estado", 4 para "mesoregiao", 5 para "microregiao", 4 para "regiaointermediaria", 6 para "regiaoimediata", 7 para "municipio".
#' @param divisao   Escopo da divisão, um de ("regiao", "estado", "mesoregiao", "microregiao", "regiaointermediaria", "regiaoimediata", "municipio").
#' @param qualidade Qualidade da malha.
#'
#' @return URL contendo arquivo GEOJSON da respectiva malha geográfica.
#' @export
#'
#' @examples
#' # mapa dos Estados do Brasil
#' mapa_url(divisao = "estado")
#' # mapa dos municípios da Paraíba
#' mapa_url(nivel = "estado", id = 25, divisao = "municipio")
mapa_url <- function(nivel = "pais", id = NULL, divisao, qualidade = "max") {
    if (nivel == "pais") id <- "BR"
    else if (is.null(id)) stop("apresente id")
    nivel2 <- switch(nivel,
        "pais"                = "/paises/",
        "regiao"              = "/regioes/",
        "estado"              = "/estados/",
        "mesoregiao"          = "/mesorregioes/",
        "microregiao"         = "/microrregioes/",
        "regiaointermediaria" = "/regioes-intermediarias/",
        "regiaoimediata"      = "/regioes-imediatas/",
        "municipio"           = "/municipios/"
    )
    divisao2 <- switch(divisao,
        "regiao"              = "&intrarregiao=regiao",
        "estado"              = "&intrarregiao=UF",
        "mesoregiao"          = "&intrarregiao=mesorregiao",
        "microregiao"         = "&intrarregiao=microrregiao",
        "regiaointermediaria" = "&intrarregiao=regiao-intermediaria",
        "regiaoimediata"      = "&intrarregiao=regiao-imediata",
        "municipio"           = "&intrarregiao=municipio"
    )
    qualidade2 <- switch(qualidade,
        "max" = "",
        "med" = "&qualidade=intermediaria",
        "min" = "&qualidade=minima"
    )
    url <- paste0(
        "https://servicodados.ibge.gov.br/api/v3/malhas",
        nivel2, id,
        "?formato=application/vnd.geo+json",
        divisao2,
        qualidade2
    )
    url
}
