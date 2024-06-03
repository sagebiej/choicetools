

#' Title function to split model into different columns for texreg
#'
#' @param condition The stub that is common for all parameters you want to split. For example 'mean'
#' @param mname The name of the model you want to split
#'
#' @return a new texreg object with only the selected column (for example the model output with only the mean parameters)
#' @export
#'
#' @examples {
#' est_model <- readRDS(system.file("extdata", "mixlogitInt_bootstrap.RDS", package = "choiceTools"))
#' ## make full model in one column using texreg
#' full_model <- quicktexregapollo(est_model, se="normal")
#' texreg::screenreg(full_model)
#' ## split the model to different columns, e.g. for mean, sd, sample_interactions
#' splitmodels <- purrr::map(c("mean_","sd_" , paste0("_s",c(2:6))  ) ,subcoef,full_model)
#' texreg::screenreg(splitmodels)
#' ## the same, but make sure gof statistics are shown only once
#' texreg::screenreg(c(splitmodels[[1]],remGOF(splitmodels[2:7] ) ) )
#'
#' }
subcoef <- function(condition, mname){

  sub <- grep(condition,methods::slot(mname,"coef.names"))

  for (ele in c("coef.names","coef","se","pvalues"))  {
    elements<- methods::slot(mname,ele)[sub]
    methods::slot(mname,ele) <- elements
  }

  methods::slot(mname,"coef.names")<-gsub(pattern = condition,replacement = "",x =methods::slot(mname,"coef.names") )

  methods::slot(mname,"model.name")<-gsub("_","",condition)


  return(mname)

}
