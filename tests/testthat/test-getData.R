currentInstance <- Sys.getenv("R_RAP_INSTANCE")
defaultW <- getOption("warn")

# make sure we can handle no error i query for real data
Sys.setenv(R_RAP_INSTANCE = "")

test_that("warning and NULL provided when no real data available", {
  expect_warning(getRealData())
  options(warn = -1)
  expect_null(getRealData())
  options(warn = defaultW)
})

test_that("function can return real data", {
  skip_if(1 == 1, message = "Postponed until data model in place. Please fix!")
  expect_true(class(getRealData()) == "list")
})

test_that("fake data can be returned", {
  expect_true(class(getFakeData()) == "list")
})

test_that("warning and NULL is provided on processing error", {
  expect_warning(processData(data = list()))
  options(warn = -1)
  expect_null(processData(data = list()))
  options(warn = defaultW)
})

test_that("fake data can be processed", {
  expect_true(class(processData(getFakeData())) == "list")
})


# restore instance
Sys.setenv(R_RAP_INSTANCE = currentInstance)
