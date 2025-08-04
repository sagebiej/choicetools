# This function lets us create output tables with texreg with objects from Apollo


#' Make your apollo object readable with texreg
#'
#' @param model the name of the apollo model object
#' @param wtpest if you want to display wtp instead of beta coefficients provide a data.frame with the wtp values and standard errors
#' @param se Which standard errors should be used. Either rob for robust, normal for normal standard error or bs for bootstrapped standard errors.
#'
#' @return a list object to be easily used in texreg. It makes it easy to create tables usind different standard errors (including robust and bootstrapped) and to display WTP instead of beta coefficients.
#' @export
#'
#' @examples \dontrun{
#' quicktexregapollo(model1)
#' }
quicktexregapollo <- function(model = model, wtpest = NULL, se = "rob") {
  if (!se %in% c("rob", "normal", "bs")) {
    stop("Invalid value for 'se'. Please use one of 'rob', 'normal', or 'bs'.")
  }


  if (se == "bs" && !"bootse" %in% names(model)) {
    stop(" It seems you did not do bootstrapping. Thus, I cannot report bootstrapped se. The 'model' object must contain an element named 'bootse' when 'se' is 'bs'.")
  }


  modelOutput_settings <- list(printPVal = T)

  if (is.null(wtpest)) {
    estimated <- janitor::clean_names(as.data.frame(apollo::apollo_modelOutput(model, modelOutput_settings)))
    switch(se,
      rob = {
        estimated$se <- estimated$rob_s_e
        estimated$pv <- estimated$p_1_sided_2
      },
      bs = {
        estimated$se <- estimated$bootstrap_s_e
        estimated$pv <- estimated$p_1_sided_3
      },
      normal = {
        estimated$se <- estimated$s_e
        estimated$pv <- estimated$p_1_sided
      },
      {
        # Default case if no match is found
        stop("Invalid value for 'se'. Please use a valid value.")
      }
    )
  } else {
    estimated <- wtpest
    colnames(estimated) <- c("estimate", "rob_s_e", "robt", "p_1_sided_2")
  }
  
  
  coefnames <- gsub(pattern = "_[a-z]$", "", rownames(estimated))
  
  texout <- createTexreg(
    coef.names = coefnames, coef = estimated[["estimate"]], se = estimated[["rob_s_e"]], pvalues = estimated$p_1_sided_2,
    gof.names = c("No Observations", "No Respondents", "Log Likelihood (Null)", "Log Likelihood (Converged)"),
    gof = c(model[["nObsTot"]], model[["nIndivs"]], model[["LL0"]][[1]], model[["LLout"]][[1]]),
    gof.decimal = c(FALSE, FALSE, TRUE, TRUE)
  )


  return(texout)
}
