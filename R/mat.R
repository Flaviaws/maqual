mat <- function(m, tam, pix, type, ...){
  d <- mean(res(m))*(tam/pix)
  fom <- focalMat(m, d, type = type)
  fom <- fom/max(fom)
}
