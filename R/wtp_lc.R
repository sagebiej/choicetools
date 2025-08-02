#' Compute willingness-to-pay for latent classes in an LC model
#'
#' This function wraps the [wtp()] function to compute WTP estimates across all classes of a latent class (LC) model estimated with the Apollo package.
#'
#' @param modelname An object returned by `apollo_estimate()` (or similar), containing the LC model results (including `LL0` and `estimate`).
#' @param pricestub A character string specifying the cost or price parameter name prefix (e.g., "cost_", "b_price_") to use in the WTP calculation. The class letter will be appended internally.
#' @param excludestub A character string specifying a parameter name prefix to exclude when subsetting coefficients (e.g., "delta_"). Defaults to "delta_." This is used in the internal grep pattern to avoid mixing in parameters from the membership function including constants.
#'
#' @return A named list. Each element (e.g. `Class 1`, `Class 2`, ...) is a `data.frame` containing:
#' \itemize{
#'   \item WTP estimates for the specified `pricestub` parameters across classes.
#'   \item Associated statistics (p-values, confidence intervals) extracted from the model output.
#' }
#'
#' @seealso [wtp()], [apollo::apollo_modelOutput()]
#'
#' @examples
#' \dontrun{
#'   # Estimate your LC model first
#'   mod <- apollo_estimate(...)
#'   # Compute WTP using default cost stub, excluding "delta_" suffix
#'   results <- wtp_lc(mod, "cost_")
#'   # Compute WTP using a price stub and custom exclusion
#'   results2 <- wtp_lc(mod, "b_price_", excludestub = "delta_")
#' }
#'
#' @export

wtp_lc <-function(modelname, pricestub, excludestub = "delta") {


  wtpvalues=list()

  for (class in 1:(length(modelname$LL0)-1)) {


    clet <- intToUtf8(96+class)
    coefs<- data.frame(apollo::apollo_modelOutput(modelname, modelOutput_settings = list(printPVal=T)))
    coefs<- coefs[grep(paste0(excludestub, ".*", clet,"$"),x = rownames(coefs), value=TRUE, perl = TRUE), c(1,5:7)]




    print(clet)

    wtpvalues[[paste0("Class ",class)]] <-
      wtp(paste0(pricestub,clet),
          grep(paste0("^(?=.*_",clet,"$)(?!.*",excludestub,")"), names(modelname$estimate) , value=T, perl = TRUE),modelname = modelname)

    colnames(coefs)<- colnames(wtpvalues[[paste0("Class ",class)]])

    wtpvalues[[paste0("Class ",class)]] <- rbind(wtpvalues[[paste0("Class ",class)]],coefs)

  }

  return(wtpvalues)

}
