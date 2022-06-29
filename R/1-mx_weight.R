#' Produz uma matriz para o focal
#'
#' @param m SpatRaster original
#' @param tam Tamanho desejado da janela do focal
#' @param pix Tamanho do pixel do SpatRaster
#' @param type Tipo de filtro que será aplicado para criar a matriz
#'             Tipos podem ser circular ("circle) ou retanular ("rectangle)
#' @return Uma matriz
#' @export
#'
#' @examples
#'  mm <- base::matrix(base::sample(1:10,
#'                     size = 100, replace = TRUE), ncol = 10, nrow = 10)
#'  map <- terra::rast(mm)
#'
#'  m <- map
#'  tam <- 100
#'  pix <- 30
#'  type <- "circle"
#'
#'  mx_weight(m, tam, pix, type)
#'
mx_weight <- function(m, tam, pix, type){
            ##
            # checando os argumentos
            stopifnot(base::inherits(m, "SpatRaster"))
            # para se "m" nao for um SpatRaster
            stopifnot(base::is.numeric(tam), base::length(tam) == 1)
            # para se "tam" nao for numerico e apenas um valor
            stopifnot(base::is.numeric(pix), base::length(pix) == 1)
            # para se "pix" nao for numerico e apenas um valor
            stopifnot(type %in% c("circle", "rectangle"))
            # para se "type" nao for igual a um dos tres tipos possiveis
            ##

            ##
            # calcula o tamanho real da janela de focal
            # de acordo com a resolução de "m",
            # tamanho do pixel (dado por "pix") e
            # tamanho desejado (dado por "tam")
            d <- base::mean(terra::res(m))*(tam/pix)
            # cria uma matriz que representara a janela do focal
            fom <- terra::focalMat(m, d, type = type)
            ##

            ##
            # transforma a matriz calculada para uma matriz de 0 e 1
            fm <- fom/max(fom)
            return(fm)  # retorna a matriz criada
            ##
          }


