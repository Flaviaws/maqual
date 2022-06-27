quality <- function(m, fm, lvl, q, na.rm){
          if (length(lvl) != length(q)){
            stop("'lvl' and 'q' must be the same lenght", call. = FALSE)
          }
          stopifnot(is.logical(na.rm), length(na.rm) == 1)

          nlevels <- function(m){  # calcula o total de níveis do spatraster
            m@ptr[["range_max"]]
          }
          l <- nlevels(m)

          pond <- function(x, lvl, q, na.rm){  # funcao para o focal
            (mean(x %in% lvl, na.rm=na.rm)*q)/l
          }
          # multiplica a média do nível pelo peso e divide pelo total de niveis

          focal(m, fm, fun=pond, lvl=lvl, q=q, na.rm=na.rm)
          # aplica "pond" com move window pela paisagem
        }



