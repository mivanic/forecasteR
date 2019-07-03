test_that("forecasting works", {
  data = rbind(
    as.data.frame(list(
      y = 4,
      x1 = 3,
      x2 = 5,
      p = 'a'
    )),
    as.data.frame(list(
      y = 2,
      x1 = 3,
      x2 = 4,
      p = 'b'
    )),
    as.data.frame(list(
      y = 4,
      x1 = 2,
      x2 = 2,
      p = 'c'
    )),
    as.data.frame(list(
      y = 6,
      x1 = 1,
      x2 = 1,
      p = 'd'
    )),
    as.data.frame(list(
      y = 7,
      x1 = 3,
      x2 = 2,
      p = 'e'
    )),
    as.data.frame(list(
      y = 2,
      x1 = 3,
      x2 = 4,
      p = 'f'
    )),
    as.data.frame(list(
      y = NA,
      x1 = 2,
      x2 = 2,
      p = 'g'
    )),
    as.data.frame(list(
      y = NA,
      x1 = 1,
      x2 = 1,
      p = 'h'
    )),
    as.data.frame(list(
      y = NA,
      x1 = NA,
      x2 = 2,
      p = 'i'
    )),
    as.data.frame(list(
      y = NA,
      x1 = NA,
      x2 = 7,
      p = 'j'
    ))
  )

  result = forecast(
    data = data,
    model = list(y = y ~ 3 * x1 + 0.1 * x1, x1 = x1 ~ 0.4 * x2 - 0.1 * y),
    periods = c('g', 'h', 'i', 'j'),
    periodColumn = 'p',
    lowerBounds = NULL,
    upperBounds = NULL,
    solver = solveR::solveSystem
  )

  expect_equal(result[10, 1], 6.625954, tolerance = 1e-5)
  expect_equal(result[9, 1], 1.893130, tolerance = 1e-5)
  expect_equal(result[9, 2], 0.610687, tolerance = 1e-5)

  result = forecast(
    data = data,
    model = list(y = y ~ 3 * x1 + 0.1 * x1, x1 = x1 ~ 0.4 * x2 - 0.1 * y),
    periods = c('g', 'h', 'i', 'j'),
    periodColumn = 'p',
    transformation = function(data) {
      data$x2 = data$x2 * 2
      return(data)
    }
    ,
    lowerBounds = NULL,
    upperBounds = NULL,
    solver = solveR::solveSystem
  )
  expect_equal(result[10, 1], 2*6.625954, tolerance = 1e-5)
  expect_equal(result[9, 1], 2*1.893130, tolerance = 1e-5)
  expect_equal(result[9, 2], 2*0.610687, tolerance = 1e-5)

})
