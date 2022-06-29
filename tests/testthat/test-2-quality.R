test_that("error is returned with wrong input class", {
  mm <- base::matrix(base::sample(1:10,
                                  size = 100, replace = TRUE), ncol = 10, nrow = 10)
  map <- terra::rast(mm)
  m <- map
  fm <- fm
  lvl <- info$lvl_cod[1]
  q <- info$q_peso[1]
  na.rm <- TRUE
  tpeso <- 55

  m1 <- c(1:10)
  expect_error(quality(m = m1, fm, lvl, q, na.rm, tpeso))

  fm1 <- 2
  expect_error(quality(m, fm = fm1, lvl, q, na.rm, tpeso))

  lvl1 <- c(1:10)
  expect_error(quality(m, fm, lvl = lvl1, q, na.rm, tpeso))

  q1 <- c(1:10)
  expect_error(quality(m, fm, lvl, q = q1, na.rm, tpeso))

  na.rm1 <- 2
  expect_error(quality(m, fm, lvl, q, na.rm = na.rm1, tpeso))

  tpeso1 <- matrix(sample(1:10, size = 100, replace = TRUE), ncol = 10, nrow = 10)
  expect_error(quality(m, fm, lvl, q, na.rm, tpeso = tpeso1))
})


test_that("returned object class is correct", {
  mm <- base::matrix(base::sample(1:10,
                     size = 100, replace = TRUE), ncol = 10, nrow = 10)
  map <- terra::rast(mm)
  m <- map
  fm <- fm
  lvl <- info$lvl_cod[1]
  q <- info$q_peso[1]
  na.rm <- TRUE
  tpeso <- 55

  expect_vector(sum(c(info$q_peso)))
  expect_s4_class(quality(m, fm, lvl, q, na.rm, tpeso), "SpatRaster")
  expect_type(quality(m, fm, lvl, q, na.rm, tpeso), "S4")
})
