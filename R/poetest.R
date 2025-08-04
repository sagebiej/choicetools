# This function lets us perform the Poe (2005) test on WTP values


#' Perform the Poe (2005) test on different discrete choice models
#'
#' @param n number of draws
#' @param model1 Model from which to take the first WTP values
#' @param model2 Model from which to take the second WTP values
#' @param att Vector of attributes whose WTP values you want to compare
#' @param price name of the price coefficient
#' @param vcov which variance-covariance matrix to use. Either normal for the normal one, or rob for the robust one.
#'
#' @return a p value associated with "WTP1>WTP2"
#' @export
#'
#' @examples \dontrun{
#' poeresults <- poetest(
#'   n = 5000, model1 = clmodels[[model_1]], model2 = clmodels[[model_2]],
#'   att = attr, price = "bcost", vcov = "normal"
#' )
#' }
poetest <- function(n, model1, model2, att, price, vcov) {
  # stop command for invalid variance covariance matrix
  if (!vcov %in% c("rob", "normal")) {
    stop("Invalid value for 'vcov'. Please use one of 'rob' or 'normal'.")
  }

  ## Extract relevant elements of models



  getalldraws <- function(n, model1, model2, att, price, vcov) {
    allmodels <- list(model1, model2)

    model_draws <- list()

    for (m in 1:2) {
      # implement the option to choose between the robust or the normal variance covariance matrix
      varcov_matrix <- switch(vcov,
        rob = allmodels[[m]][["robvarcov"]],
        normal = allmodels[[m]][["varcov"]],
        stop("Invalid value for 'vcov'. Please use one of 'rob' or 'normal'.")
      )

      model_draws[[m]] <- takedraws(n, allmodels[[m]][["estimate"]], varcov_matrix)

      model_draws[[m]] <- cbind(model_draws[[m]], wtp = model_draws[[m]][, att] / model_draws[[m]][, price])
    }

    return(model_draws)
  }


  draws <- getalldraws(n, model1, model2, att, price, vcov)

  wtpvec <- cbind(wtp1 = draws[[1]][, "wtp"], wtp2 = draws[[2]][, "wtp"])



  # fullconv = matrix(ncol =2, nrow = 0)
  # for (i in 1:nrow(wtpvec)) {
  #
  # fullconv= rbind(fullconv , cbind(rep(wtpvec[i,1], times=nrow(wtpvec))  , wtpvec[,2] ) )
  # }


  # fullconv = expand.grid(wtpvec[,1], wtpvec[,2])
  fullconv <- expand.grid(as.data.frame(wtpvec))



  results <- cbind(fullconv, fullconv[, 1] > fullconv[, 2])

  cat("\n The probability that WTP_1 > WTP2 is ", mean(results[, 3]), "\n The probability that WTP_2 > WTP1 is ", 1 - mean(results[, 3]))

  output <- list(Allcomparisions = results, p1 = mean(results[, 3]), p2 = 1 - mean(results[, 3]), No_draws = n)

  return(output)
}
