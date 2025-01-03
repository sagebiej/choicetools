# test_that("quicktexregapollo function handles invalid model class", {
#   invalid_model <- list()
#   class(invalid_model) <- c("invalidClass")
# 
#   expect_error(
#     quicktexregapollo(model = invalid_model, se = "rob"),
#     "Invalid model class. The model must be of classes 'apollo', 'maxLik', and 'maxim'."
#   )
# })




test_that("quicktexregapollo function handles invalid 'se' argument", {
  model_path <- system.file("extdata", "mixlogitInt_bootstrap.RDS", package = "choiceTools")
  est_model <- readRDS(model_path)

  expect_error(
    quicktexregapollo(model = est_model, se = "invalid"),
    "Invalid value for 'se'. Please use one of 'rob', 'normal', or 'bs'."
  )
})


test_that("quicktexregapollo function checks for 'bootse' element when se is 'bs'", {
  model_path <- system.file("extdata", "mixlogitInt_bootstrap.RDS", package = "choiceTools")
  est_model <- readRDS(model_path)
  est_model$bootse <- NULL  # Remove 'bootse' element

  expect_error(
    quicktexregapollo(model = est_model, se = "bs"),
    "It seems you did not do bootstrapping. Thus, I cannot report bootstrapped se. The 'model' object must contain an element named 'bootse' when 'se' is 'bs'."
  )
})

