model_path <- system.file("extdata", "mixlogitInt_bootstrap.RDS", package = "choiceTools")
test_that("mixlogitInt_bootstrap.RDS exists", {
  expect_true(file.exists(model_path), info = "mixlogitInt_bootstrap.RDS must be in inst/extdata")
})

# 2) Read in the model
model <- readRDS(model_path)

# 3) Define cost and attribute vectors
cost_param <- "mean_b_beitrag"
attr_params <- grep("^mean_b_", names(model$estimate), value = TRUE)

test_that("wtp returns correct structure", {
  res <- wtp(cost_param, attr_params, model)

  # Columns
  expect_s3_class(res, "data.frame")
  expect_equal(
    colnames(res),
    c("wtp", "robse", "robt", "pVal"),
    info = "wtp() must return exactly these four columns"
  )

  # Row names
  expected_rows <- setdiff(attr_params, cost_param)
  expect_setequal(
    rownames(res),
    expected_rows
  )

  # p-values in [0,1]
  expect_true(
    all(res$pVal >= 0 & res$pVal <= 1)
  )
})


test_that("wtp computes the analytic ratio correctly", {
  # Only one non-cost attribute
  single_attr <- "mean_b_groesse"
  res2 <- wtp(cost_param, c(cost_param, single_attr), model)

  # Manual ratio: - β_groesse / β_beitrag, stripped of names
  manual_wtp <- -model$estimate[single_attr] / model$estimate[cost_param]
  manual_wtp <- unname(manual_wtp)

  # Extract the computed WTP (already numeric)
  computed_wtp <- res2[single_attr, "wtp"]

  expect_equal(
    computed_wtp,
    manual_wtp,
    tolerance = 1e-4
  )
})




test_that("mediancost = TRUE applies the exp() transform", {
  single_attr <- "mean_b_groesse"
  res_med <- wtp(cost_param, c(cost_param, single_attr), model, mediancost = TRUE)

  # Manual median-based ratio: β_groesse / exp(β_beitrag)
  manual_med <- model$estimate[single_attr] / exp(model$estimate[cost_param])
  manual_med <- unname(manual_med)

  computed_med <- res_med[single_attr, "wtp"]

  expect_equal(
    computed_med,
    manual_med,
    tolerance = 1e-3
  )
})

test_that("error if cost parameter not in attr vector", {
  # pick a param that definitely isn't there
  expect_error(
    wtp("not_a_param", attr_params, model),
    regexp = "SYNTAX ISSUE - The expression mean_b_groesse",
    info = "Calling wtp() with a cost name outside of attr should error"
  )
})
