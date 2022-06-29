test_that("error is returned with wrong input class", {
  mm <- base::matrix(base::sample(1:10,
                                  size = 100, replace = TRUE), ncol = 10, nrow = 10)
  map <- terra::rast(mm)
  m <- map
  fm <- fm
  lvl <- info$lvl_cod[1]
  q <- info$q_peso[1]
  na.rm <- TRUE

  m1 <- c(1:10)
  expect_error(quality(m = m1, fm, lvl, q, na.rm))

  fm1 <- 2
  expect_error(quality(m, fm = fm1, lvl, q, na.rm))

  lvl1 <- c(1:10)
  expect_error(quality(m, fm, lvl = lvl1, q, na.rm))

  q1 <- c(1:10)
  expect_error(quality(m, fm, lvl, q = q1, na.rm))

  na.rm1 <- 2
  expect_error(quality(m, fm, lvl, q, na.rm = na.rm1))
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

  expect_vector(sum(c(info$q_peso)))
  expect_s4_class(quality(m, fm, lvl, q, na.rm), "SpatRaster")
  expect_type(quality(m, fm, lvl, q, na.rm), "S4")
})
