#' Calcula a qualidade da matriz
#'
#' @param m SpatRaster original
#' @param fm Objeto de matriz numérica para o "move window" do focal
#' @param lvl Número do nível (ou elemento da matriz) que será calculado a qualidade
#' @param q Valor de peso para calcular a qualidade de lvl
#' @param na.rm Argumento lógico
#'
#' @return Um SpatRaster
#' @export
#'
#' @examples
#' mm <- base::matrix(base::sample(1:10,
#'                    size = 100, replace = TRUE), ncol = 10, nrow = 10)
#' map <- terra::rast(mm)
#'
#' fm <- base::matrix(c(0, 0, 1, 1, 1, 0, 0,
#'                      0, 1, 1, 1, 1, 1, 0,
#'                      1, 1, 1, 1, 1, 1, 1,
#'                      1, 1, 1, 1, 1, 1, 1,
#'                      1, 1, 1, 1, 1, 1, 1,
#'                      0, 1, 1, 1, 1, 1, 0,
#'                      0, 0, 1, 1, 1, 0, 0),
#'                    ncol = 7, nrow = 7)
#'
#' info <- base::data.frame(lvl_cod = c(1:10),
#'                          habitat = c("Floresta madura", "Floresta incial",
#'                                      "Savana", "Pastagem",
#'                                      "Agricultura", "Silvicultura",
#'                                      "N-vegetada", "Urbano",
#'                                      "Mineiracao", "Agua"),
#'                           q_peso = c(10:1))
#'
#' m <- map
#' fm <- fm
#' lvl <- info$lvl_cod[1]
#' q <- info$q_peso[1]
#' na.rm <- TRUE
#'
#' quality(m, fm, lvl, q, na.rm)
#'
quality <- function(m, fm, lvl, q, na.rm){
          ##
          # checando os argumentos
          stopifnot(base::inherits(m, "SpatRaster"))
          # para se "m" nao for um SpatRaster
          stopifnot(base::is.matrix(fm))
          # para se "fm" nao for uma matriz
          if (base::length(lvl) != base::length(q)){
            stop("'lvl' and 'q' must be the same lenght", call. = FALSE)
          }
          # para se "lvl" e "q" nao tiverem o mesmo comprimento e avisa o erro
          stopifnot(base::is.logical(na.rm), base::length(na.rm) == 1)
          # para se "na.rm" nao for uma argumento logico e se tiver comprimento
          # diferente de 1
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
          return(r)
          # retorna um SpatRaster r com os resultados do focal para
          # o nível de habitat definido
          ##
        }



