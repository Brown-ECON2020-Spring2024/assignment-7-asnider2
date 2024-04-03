test_that("This is an example test", {
  a <- 2
  b <- 10
  expect_equal(a * b, 20)
})

test_that("Check estimated coefficient for 'gdpPercap'", {
  expect_equal(unname(coef(model)["gdpPercap"]), 0.00076, tolerance = 1e-5)
})

test_that("Check if the number of observations in the model is equal to 1704", {
  expect_equal(nobs(model), 1704)
})

test_that("Check data validation", {
  expect_false(length(unique(paste(data$year, data$country))) == nrow(data))
  expect_true(all(data$gdpPercap >= 0 & data$gdpPercap <= 1e6))
  expect_true(all(data$continent %in% c("Asia", "Europe", "Africa", "Americas", "Oceania"))) # nolint
})