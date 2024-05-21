# This function lets us create output tables with texreg with objects from Apollo


#' Make your apollo object readable with texreg
#'
#' @param model the name of the apollo model object
#' @param wtpest if you want to display wtp instead of beta coefficients provide a dataframe with the wtp values and standard errors
#'
#' @return a list opject for texreg
#' @export
#'
#' @examples
quicktexregapollo <- function(model = model, wtpest = NULL, se="rob") {

  modelOutput_settings = list(printPVal=T)

  if (is.null(wtpest)) {  estimated <- janitor::clean_names(as.data.frame(apollo_modelOutput(model, modelOutput_settings)))
  } else{
    estimated <- wtpest
    colnames(estimated)<- c("estimate", "rob_s_e", "robt", "p_1_sided_2")

  }




  coefnames <- gsub(pattern = "_[a-z]$", "" ,rownames(estimated))

  texout <- texreg::createTexreg(coef.names = coefnames , coef = estimated[["estimate"]] , se = estimated[["rob_s_e"]] , pvalues = estimated$p_1_sided_2,
                         gof.names = c("No Observations" , "No Respondents" , "Log Likelihood (Null)" , "Log Likelihood (Converged)") ,
                         gof = c(model[["nObsTot"]] , model[["nIndivs"]], model[["LL0"]][[1]] , model[["LLout"]][[1]] ) ,
                         gof.decimal = c(FALSE,FALSE,TRUE,TRUE)
  )


  return(texout)

}
