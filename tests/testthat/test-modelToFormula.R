test_that("conversion works", {
  data = rbind(
    as.data.frame(list(y=4,x1=3, x2=5)),
    as.data.frame(list(y=2,x1=3, x2=4)),
    as.data.frame(list(y=4,x1=2, x2=2)),
    as.data.frame(list(y=6,x1=1, x2=1)),
    as.data.frame(list(y=7,x1=3, x2=2)),
    as.data.frame(list(y=2,x1=3, x2=4)),
    as.data.frame(list(y=4,x1=2, x2=2)),
    as.data.frame(list(y=6,x1=1, x2=1)),
    as.data.frame(list(y=7,x1=3, x2=2)),
    as.data.frame(list(y=3,x1=2, x2=7))
  )

  frm=y~x1+x2
  model = lm(data=data, formula=frm)
  result = modelToFormula(model)
  expect_equal(result, y ~ 6.30688448074679 * (Intercept) + 0.0758459743290548 * x1 + -0.660443407234539 * x2)
})


test_that("conversion works for complex right side", {
  data = rbind(
    as.data.frame(list(y=4,x1=3, x2=5)),
    as.data.frame(list(y=2,x1=3, x2=4)),
    as.data.frame(list(y=4,x1=2, x2=2)),
    as.data.frame(list(y=6,x1=1, x2=1)),
    as.data.frame(list(y=7,x1=3, x2=2)),
    as.data.frame(list(y=2,x1=3, x2=4)),
    as.data.frame(list(y=4,x1=2, x2=2)),
    as.data.frame(list(y=6,x1=1, x2=1)),
    as.data.frame(list(y=7,x1=3, x2=2)),
    as.data.frame(list(y=3,x1=2, x2=7))
  )

  frm=log(y)~log(x1)+log(x2)
  model = lm(data=data, formula=frm)
  result = modelToFormula(model)
  expect_equal(result, log(y) ~ 1.79806242808605 * (Intercept) + 0.20660887895783 * log(x1) + -0.596233883441901 * log(x2))
})
