mx_weight <- function(m, tam, pix, type, ...){
          d <- mean(terra::res(m))*(tam/pix)
          fom <- terra::focalMat(m, d, type = type)
          fom <- fom/max(fom)
        }


