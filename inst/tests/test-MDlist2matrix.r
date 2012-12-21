context("MDlist2matrix")

test_that("MDlist2matrix works with lists", {
  x1 <- list(disc=c(.2, .4, .2), EE=c(.67, .54), PA=c(.86), DP=c())
  x2 <- list(disc=c(.6), BO=c())
  x3 <- list(disc=c(), EE=c(.2), PA=c(.4, .67), DP=c( .2, .54, .86))
  x4 <- list(disc=c(.2, .4, .2), EE=c(), PA=c(), DP=c())
  x5 <- list(disc=c(), EE=c(.2), PA=c(.4), DP=c(.2))

  expect_is(MDlist2matrix(x1), "full_matrix")
  expect_is(MDlist2matrix(x2), "full_matrix")
  expect_is(MDlist2matrix(x3), "full_matrix")
  expect_is(MDlist2matrix(x4), "incomplete_matrix")
  expect_is(MDlist2matrix(x5), "incomplete_matrix")
})
