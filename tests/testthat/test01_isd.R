context("Simple first test")

dat1 <- neonbecs::get_toy_portal_data()

test_that("making an ISD works", {

  isd <-dat1 %>%
    replicatebecs::add_energy_sizeclass() %>%
    neonbecs::make_isd()

  expect_true(is.data.frame(isd))
  expect_false(anyNA(isd))
  expect_true(colnames(isd)[7] == "ln_energy")

})
