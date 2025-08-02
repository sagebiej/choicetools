#' Compute willingness-to-pay (WTP) for a single attribute
#'
#' Calculates point estimates, standard errors, and confidence intervals for willingness-to-pay 
#' of specified attributes in a discrete choice model via the delta method. Optionally, uses 
#' the median cost transformation when `mediancost = TRUE`.
#'
#' @param cost A string naming the cost (denominator) parameter, e.g. "cost_a" or "price_b".
#' @param attr A character vector of attribute parameter names for which to compute WTP. 
#'   The `cost` name will be removed from this vector internally.
#' @param modelname A model object (e.g. from `apollo_estimate()`) containing at least 
#'   components `estimate` (named coefficients) and `vcov` (variance-covariance matrix) used by `apollo_deltaMethod()`.
#' @param mediancost Logical. If `TRUE`, transforms the cost parameter via `-exp(cost)` before
#'   ratio computation. Defaults to `FALSE`.
#'
#' @return A `data.frame` with row names equal to the remaining `attr` entries and columns:
#' \describe{
#'   \item{wtp}{Point estimate of willingness-to-pay (ratio of coefficients).}
#'   \item{robse}{Standard error of the WTP estimate (via delta method).}
#'   \item{robt}{Z-statistic for the WTP estimate.}
#'   \item{pVal}{Two-sided p-value computed from `robt`.}
#' }
#'
#' @details
#' For each attribute `a` in `attr`, computes:
#' \eqn{WTP_a = - \beta_a / \beta_cost}
#' and uses `apollo::apollo_deltaMethod()` to derive SEs and CIs via the delta method.
#'
#' @examples
#' \dontrun{
#'   # model <- apollo_estimate(...)
#'   wtp_values <- wtp("cost_a", c("time_b", "time_c"), model)
#'   wtp_values_median <- wtp("cost_a", c("time_b", "time_c"), model, mediancost = TRUE)
#' }
#'
#' @export
wtp <- function(cost, attr, modelname, mediancost = FALSE) {
  wtp_values <- data.frame(
    wtp = numeric(length(attr) - 1),
    robse = numeric(length(attr) - 1),
    robt = numeric(length(attr) - 1),
    stringsAsFactors = FALSE
  )
  # remove the cost element from attr
  attr_names <- setdiff(attr, cost)
  
  # optionally transform cost to median-based expression
  cost_expr <- cost
  if (mediancost) {
    cost_expr <- paste0("-exp(", cost, ")")
  }
  
  for (i in seq_along(attr_names)) {
    a <- attr_names[i]
    expr <- paste0(a, " / ", cost_expr)
    # call delta method
    dm <- apollo::apollo_deltaMethod(
      modelname,
      list(expression = expr)
    )
    # extract estimate, se, z
    wtp_values[i, ] <- dm[, 2:4]
  }
  
  # assign row names and adjust signs
  rownames(wtp_values) <- attr_names
  wtp_values$wtp <- -wtp_values$wtp
  wtp_values$pVal <- 2 * (1 - stats::pnorm(abs(wtp_values$robt)))
  
  return(wtp_values)
}
