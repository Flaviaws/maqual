#' Calcula a qualidade da matriz
#'
#' @param m SpatRaster original
#' @param fm Objeto de matriz numérica para o "move window" do focal
#' @param lvl Número do nível (ou elemento da matriz) que será calculado a qualidade
#' @param q Valor de peso para calcular a qualidade de lvl
#' @param na.rm Argumento lógico
#' @param hab Argumento do tipo "character" que representa o nome do habitat
#'            que sera considerado em "lvl
#'
#' @return Um SpatRaster
#' @export
#'
#' @examples
#' map <- mapa_teste
#'
#' map2 <- base::matrix(c(1,1, # floresta
#'                        3,1,
#'                        4,1,
#'                        5,1,
#'                       49,1,
#'                       10,2, # natural nao-florestal
#'                       11,2,
#'                       12,2,
#'                       32,2,
#'                       29,2,
#'                       13,2,
#'                       14,3, # pastagem
#'                       15,3,
#'                       18,4, # agricultura
#'                       19,4,
#'                       39,4,
#'                       20,4,
#'                       40,4,
#'                       41,4,
#'                       36,4,
#'                       46,4,
#'                       47,4,
#'                       48,4,
#'                        9,5, # silvicultura
#'                       21,3,
#'                       22,6, # nao-vegetada
#'                       23,6,
#'                       24,7, # urbano
#'                       30,8, # mineira??o
#'                       25,6,
#'                       26,9, # agua
#'                       33,9,
#'                       31,9,
#'                       27,7), ncol = 2, byrow = TRUE)
#'
#' map_C <- terra::classify(map, map2)
#'
#' fm <- maqual::mx_weight(map_c, 100, 30, "circle")
#'
#' info <- base::data.frame(lvl_cod = c(1:9),
#'                          habitat = c("Floresta", "Natural n-florestal", "Pastagem",
#'                                      "Agricultura", "Silvicultura", "N-vegetada",
#'                                      "Urbano", "Mineiracao", "Agua"),
#'                           q_peso = c(9:1))
#'
#' m <- map_c
#' fm <- fm
#' lvl <- info$lvl_cod[1]
#' q <- info$q_peso[1]
#' na.rm <- TRUE
#' hab <- info$habitat[1]
#'
quality <- function(m, fm, lvl, q, na.rm, hab){
          ##
          # checando os argumentos
          if(base::inherits(m, "SpatRaster", which = FALSE)){
            stop("Argument 'm' must be an object of class of 'SpatRaster'")
          }
          # para se "m" nao for um SpatRaster e avisa o erro
          stopifnot(base::is.matrix(fm))
          # para se "fm" nao for uma matriz
          if (base::length(lvl) != base::length(q)){
            stop("'lvl' and 'q' must be the same lenght", call. = FALSE)
          }
          # para se "lvl" e "q" nao tiverem o mesmo comprimento e avisa o erro
          stopifnot(base::is.logical(na.rm), base::length(na.rm) == 1)
          # para se "na.rm" nao for uma argumento logico e se tiver comprimento
          # diferente de 1
          stopifnot(base::is.character(hab))
          # para se "hab" não for um objeto do tipo caracter, ou seja,
          # algo como o nome do habitat que sera considerado em "lvl"
          ##

          ##
          # calcula o total de níveis do spatraster
          nlevels <- function(m){
            m@ptr[["range_max"]]
          }
          l <- nlevels(m)  # salva em um novo objeto
          ##

          ##
          # cria a funcao que sera utilizada no focal
          pond <- function(x, lvl, q, na.rm){
            (base::mean(x %in% lvl, na.rm = na.rm)*q)/l
          }
          # multiplica a média do nível pelo peso e divide pelo total de niveis
          ##

          ##
          # aplica o focal ao m utlizando a funcao pond
          r <- terra::focal(m, fm, fun = pond, lvl = lvl, q = q, na.rm = na.rm)
          return(graphics::plot(r, main = hab))
          # retorna o plot do SpatRaster r com os resultados do focal para
          # o nível de habitat definido
          ##
        }



