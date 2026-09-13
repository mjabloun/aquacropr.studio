test_that("aquacropr_studio_app returns a Shiny app object", {
  skip_if_not_installed("aquacropr")
  skip_if_not_installed("shiny")
  app <- aquacropr_studio_app()
  expect_s3_class(app, "shiny.appobj")
})
