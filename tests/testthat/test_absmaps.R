library(absmaps)

context("Test that local file can be loaded and tidied")

path <- "../testdata"

test_that("Can we load absmaps data?",{

  load_absmaps(statisticalArea = "gcc",
               year = 2011,
               saveDirectory = path,
               download = F)

  expect_is(gcc2011, "sf")

})


test_that("Can we load multiple absmaps years data",{

  load_absmaps(statisticalArea = "gcc",
               year = c(2011, 2016),
               saveDirectory = path,
               download = F)

  expect_is(gcc2011, "sf")
  expect_is(gcc2016, "sf")

})


test_that("Can we load multiple absmaps areas data",{

  load_absmaps(statisticalArea = c("sa4", "gcc"),
               year = 2016,
               saveDirectory = path,
               download = F)

  expect_is(gcc2011, "sf")
  expect_is(gcc2016, "sf")

})

test_that("Can load data and manipulate it",{

  load_absmaps(statisticalArea = "gcc",
               year = 2016,
               saveDirectory = path,
               download = F)

  testdata <-
  gcc2016 %>%
    rename(hello = state)

  expect_is(testdata, "sf")

})


# Second test takes longer so do later
test_that("Can we download absmaps data?",{

  skip_on_cran()

  load_absmaps(statisticalArea = "sa4",
               year = 2016,
               saveDirectory = path,
               download = T)

  expect_is(sa42016, "sf")

})
