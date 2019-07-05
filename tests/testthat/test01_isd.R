context("Simple first test")

dat1 <- get_toy_portal_data()

test_that("making an ISD works", {

  isd <-dat1 %>%
    make_isd()

  expect_true(is.data.frame(isd))
  expect_false(anyNA(isd))

})
