test_that("error is returned with wrong input class", {
  mm <- base::matrix(base::sample(1:10,
                                  size = 100, replace = TRUE), ncol = 10, nrow = 10)
  map <- terra::rast(mm)
  m <- map
  tam <- 100
  pix <- 30
  type <- "circle"

  m1 <- c(1:10)
  expect_error(mx_weight(m = m1, tam, pix, type))

  tam1 <- c(1:10)
  expect_error(mx_weight(m, tam = tam1, pix, type))

  tam2 <- "circle"
  expect_error(mx_weight(m, tam = tam2, pix, type))

  pix1 <- c(1:10)
  expect_error(mx_weight(m, tam, pix = pix1, type))

  pix2 <- "circle"
  expect_error(mx_weight(m, tam, pix = pix2, type))

  type1 <- c(1:10)
  expect_error(mx_weight(m, tam, pix, type = type1))

  type2 <- "line"
  expect_error(mx_weight(m, tam, pix, type = typ2))
})


test_that("returned object class is correct", {
  mm <- base::matrix(base::sample(1:10,
                                  size = 100, replace = TRUE), ncol = 10, nrow = 10)
  map <- terra::rast(mm)
  m <- map
  tam <- 100
  pix <- 30
  type <- "circle"

  expect_vector(base::mean(terra::res(m))*(tam/pix))
  d <- 1
  expect_vector(terra::focalMat(m, d, type = type))
  expect_vector(mx_weight(m, tam, pix, type))

  expect_type(base::mean(terra::res(m))*(tam/pix), "double")
  d <- 1
  expect_type(terra::focalMat(m, d, type = type), "double")
  expect_type(mx_weight(m, tam, pix, type), "double")
})





