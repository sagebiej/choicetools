# This function lets us create output tables with texreg with objects from Apollo


quicktexregapollo <- function(model =model, wtpest=NULL) {

  modelOutput_settings = list(printPVal=T)

  if (is.null(wtpest)) {  estimated <- janitor::clean_names(as.data.frame(apollo_modelOutput(model, modelOutput_settings)))
  } else{
    estimated <- wtpest
    colnames(estimated)<- c("estimate", "rob_s_e", "robt", "p_1_sided_2")

  }


  coefnames <- gsub(pattern = "_[a-z]$", "" ,rownames(estimated))

  texout <- createTexreg(coef.names = coefnames , coef = estimated[["estimate"]] , se = estimated[["rob_s_e"]] , pvalues = estimated$p_1_sided_2,
                         gof.names = c("No Observations" , "No Respondents" , "Log Likelihood (Null)" , "Log Likelihood (Converged)") ,
                         gof = c(model[["nObsTot"]] , model[["nIndivs"]], model[["LL0"]][[1]] , model[["LLout"]][[1]] ) ,
                         gof.decimal = c(FALSE,FALSE,TRUE,TRUE)
  )


  return(texout)

}
