#' @title Converter informações sobre Estados
#' @description Converte entre IDs, Nome, Sigla, Capital ou Região de um Estado.
#'
#' @param x    Entrada a ser convertida.
#' @param de   Caracter indicando o tipo de entrada. Deve ser um de: "id", "nome", "sigla" ou "capital".
#' @param para Caracter indicando o tipo de retorno. Deve ser um de: "id", "nome", "sigla", "capital" ou "regiao".
#'
#' @return Vetor do tipo de retorno selecionado, contendo a conversão das informações.
#' @export
#'
#' @examples
#' converter_estado("TO", de = "sigla", para = "nome")
#' converter_estado(unique(mapa_estado$id), de = "id", para = "nome")
converter_estado <- function(x, de, para) {
    sapply(x,
        function(x) {
            brhelper::dados_estado[[para]][brhelper::dados_estado[[de]] == x]
        }
    )
}
