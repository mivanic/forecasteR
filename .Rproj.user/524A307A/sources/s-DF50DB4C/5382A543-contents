test_that("Estimate model works", {
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

  result = estimateModel(data = data,
                         formulas = list(formula1 = x1 ~ y, formula2 = x2 ~ y))

  expect_equal(length(result), 2)
  expect_equal(class(result), 'list')
  expect_equal(names(result), c('formula1', 'formula2'))

})
