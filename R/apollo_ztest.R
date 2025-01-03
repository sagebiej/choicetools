#' Perform a z test to compare parameters from two models which were estimated with apollo
#'
#'Use this test if you estimate the exact same model with different data sets. For example, if you have a split sample and want to compare the two samples for differences in preferences. The test is especially useful for models in WTP Space where the Poe Test ist not adequate.
#' @param model1 Model from which to take the first parameter
#' @param model2 Model from which to take the second parameter
#' @param hyp Your hypotheses to test. Default is 0 which means you test for equality of parameters. Currently there are no other options. If you change the value, it will still test for equality (0)
#'
#' @return a dataframe with the test statistics
#' @export
#'
#' @examples \dontrun{
#'  apollo_ztest(model1,model2)
#'  }
apollo_ztest <- function(model1, model2, hyp=0){

  comp = data.frame(m1par =model1[["estimate"]] ,m2par = model2[["estimate"]] , m1se=model1[["robse"]] , m2se=model2[["robse"]]) %>%

    dplyr::mutate(diffmean=m1par-m2par , error= sqrt(m1se^2+m2se^2) , z= diffmean/error , p_value=2*stats::pnorm(-abs(z)))


  print(comp)

  cat(class(comp))

  return(comp)

}
