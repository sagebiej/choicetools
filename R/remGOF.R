#' Remove unnecessary statistics from Table for TexReg
#'
#' @param models the models you want to delete the GOF statistics
#'
#' @return a list with the same models as in models but without GOF statistics
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
#' }
remGOF<- function(models){

  gof<- function(m){
    methods::slot(m,"gof.names")<- character(0)
    methods::slot(m,"gof")<- numeric(0)
    methods::slot(m,"gof.decimal")<- logical(0)

    return(m)
  }

  return(purrr::map(models,gof))

}
