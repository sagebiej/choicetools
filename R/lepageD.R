#' Perform the Lepage Two-Sample Location-Scale Test with Simulation
#'
#' This function simulates data from two estimated models and applies the 
#' Lepage two-sample location-scale test to assess differences in location 
#' and/or scale between two distributions.
#'
#' @param sim Integer. The number of simulations to run.
#' @param n Integer. The number of samples to draw for each simulation.
#' @param modelname1 A list containing `estimate` (mean vector) and `varcov` (variance-covariance matrix) for the first model. Can be an apollo model object. 
#' @param modelname2 A list containing `estimate` (mean vector) and `varcov` (variance-covariance matrix) for the second model. Can be an apollo model object. 
#' @param meanname Character. The name of the column representing the mean parameter in the models.
#' @param sdname Character. The name of the column representing the standard deviation parameter in the models.
#'
#' @return A list of Lepage test results from each simulation, containing test statistics and p-values.
#'
#' @details
#' The function:
#' 1. Generates parameter draws using `takedraws()`.
#' 2. Simulates normal distributions based on these parameters.
#' 3. Applies the Lepage two-sample location-scale test using `lepage.test()`.
#' 4. Returns a list of results from each simulation.
#'
#' @examples
#' model1 <- list(estimate = c(mean = 1.5, sd = 0.5), varcov = matrix(c(0.1, 0.02, 0.02, 0.2), nrow = 2))
#' model2 <- list(estimate = c(mean = 1.8, sd = 0.6), varcov = matrix(c(0.12, 0.03, 0.03, 0.25), nrow = 2))
#' results <- lepageD(sim = 100, n = 50, modelname1 = model1, modelname2 = model2, meanname = "mean", sdname = "sd")
#' head(results)
#'
#' @export
lepageD <- function(sim, n, modelname1, modelname2, meanname, sdname) {  
 
  
  #********************************************************************
  ### LEPAGE TEST FUNCTIONS (FROM NSM3 PACKAGE)
  
  lepage.test <- function(x, y = NA, g = NA, method = NA, n.mc = 10000){
    
    ##Adapted from kruskal.test()##
    if (is.list(x)) {
      if (length(x) < 2L) 
        stop("'x' must be a list with at least 2 elements")
      y <- x[[2]]
      x <- x[[1]]
    }
    else {
      if(min(is.na(y)) != 0){
        k <- length(unique(g))
        if (length(x) != length(g)) 
          stop("'x' and 'g' must have the same length")
        if (k < 2) 
          stop("all observations are in the same group")
        y <- x[g == 2]
        x <- x[g == 1]
      }
    }
    #####################
    
    outp <- list()
    outp$m <- m <- length(x)
    outp$n <- n <- length(y)
    outp$n.mc <- n.mc
    N <- outp$m + outp$n
    outp$ties <- (length(c(x, y)) != length(unique(c(x, y))))
    even <- (outp$m + outp$n + 1)%%2
    outp$stat.name <- "Lepage D"
    
    
    ##When the user doesn't give us any indication of which method to use, try to pick one.
    if(is.na(method)){
      if(choose(outp$m + outp$n, outp$n) <= 10000){
        method <- "Exact"
      }
      if(choose(outp$m + outp$n, outp$n) > 10000){
        method <- "Monte Carlo"
      }
    }
    #####################################################################
    outp$method <- method
    
    tmp.W <- rank(c(x, y))
    
    our.data <- rbind(c(x, y), c(rep(1, length(x)), rep(0, length(y))))
    sorted <- our.data[1, order(our.data[1, ]) ]
    x.labels <-our.data[2, order(our.data[1, ]) ]
    
    med <- ceiling(N / 2)
    if(even){no.ties <- c(1:med, med:1)}
    if(!even){no.ties <- c(1:med, (med - 1):1)}
    
    obs.group <- numeric(N)
    group.num <- 1
    for(i in 1:N){
      if(obs.group[i] == 0){
        obs.group[i] <- group.num
        for(j in i:N){
          if(sorted[i] == sorted[j]){
            obs.group[j] <- obs.group[i]
          }
        }
        group.num <- group.num + 1;
      }
    }
    
    group.ranks <- tapply(no.ties, obs.group, mean)
    
    tied.ranks <- numeric(N)
    for(i in 1:group.num){
      tied.ranks[which(obs.group == as.numeric(names(group.ranks)[i]))] <- group.ranks[i]
    }
    
    tmp.C <- c(tied.ranks[x.labels == 1], tied.ranks[x.labels == 0])
    
    ##Only needs to depend on y values
    D.calc <- function(C.vals, W.vals){
      
      if(even){
        exp.C <- n * (N + 2) / 4
        var.C <- m * n * (N + 2) * (N - 2) / (48 * (N - 1))
      }
      if(!even){
        exp.C <- n * (N + 1)^2 / (4 * N)
        var.C <- m * n * (N + 1) * (3 + N^2) / (48 * N^2)
      }
      W.obs <- sum(W.vals)
      W.star <- (W.obs - n * (N + 1) / 2) / sqrt(m * n * (N + 1) / 12)
      C.star <- (sum(C.vals) - exp.C) / sqrt(var.C)
      return(W.star^2 + C.star^2)
    }
    
    outp$obs.stat <- D.calc(tmp.C[(m + 1):N], tmp.W[(m + 1):N])
    
    if(outp$method == "Exact"){
      possible.orders <- gtools::combinations(outp$m + outp$n, outp$n)
      
      possible.C <- t(apply(possible.orders, 1, function(x) tmp.C[x]))
      possible.W <- t(apply(possible.orders, 1, function(x) tmp.W[x]))
      
      theor.dist <- numeric(nrow(possible.C))
      for(i in 1:nrow(possible.C)){
        theor.dist[i] <- D.calc(possible.C[i, ], possible.W[i, ])
      }
      
      outp$p.value <- mean(theor.dist >= outp$obs.stat)
    }
    
    if(outp$method == "Asymptotic"){
      outp$p.value <- (1 - pchisq(outp$obs.stat, 2))
    }
    
    if(outp$method == "Monte Carlo"){
      outp$p.value <- 0
      for(i in 1:n.mc){
        mc.sample <- sample(1:N, n)
        
        if(D.calc(tmp.C[mc.sample], tmp.W[mc.sample]) >= outp$obs.stat){
          outp$p.value = outp$p.value + 1 / n.mc
        }
      }
    }
    
    cat("\nLepage two-sample location-scale test\n")
    cat("\nNull hypothesis: The locations and scales of the two population distributions are equal.\n")
    cat("Alternative hypothesis: The locations and/or scales of the two population distributions differ.\n")
    cat(paste("\nD = ", round(outp$obs.stat, 3), ", p-value = ", round(outp$p.value, 4), "\n\n", sep=""))
    
    #class(outp)="NSM3Ch5p"
    return(outp)
  }
  
  #########********************************************************************
  
   
  values <- data.frame(p = double(sim), D = double(sim))
  
  m1 <- takedraws(n=sim , modelname1$estimate, modelname1$varcov)
  m2 <- takedraws(n=sim , modelname2$estimate, modelname2$varcov)

  

  drawfromnorm <- function(s,n,meanname,sdname) {

    x <- rnorm(n, mean = m1[s,meanname], sd = abs(m1[s,sdname]))
    y <- rnorm(n, mean = m2[s,meanname], sd = abs(m2[s,sdname]))
  
    test <- lepage.test(x, y)
    
    return(test)
  }
    
  testresults <- purrr::map(seq_along(1:sim), ~drawfromnorm(s=., n=n,meanname=meanname,sdname=sdname))
  
  
  # for (i in 1:n_sim) {
  #   # Sample means and standard deviations from their respective distributions
  #   mean_x <- rnorm(1, mean = mu_x, sd = se_mu_x)
  #   mean_y <- rnorm(1, mean = mu_y, sd = se_mu_y)
  #   
  #   sd_x <- rnorm(1, mean = sigma_x, sd = se_sigma_x)
  #   sd_y <- rnorm(1, mean = sigma_y, sd = se_sigma_y)
  #   
  #   # Ensure standard deviations are positive
  #   sd_x <- max(sd_x, 0.01)
  #   sd_y <- max(sd_y, 0.01)
  #   
  #   # Generate data
  #   x <- rnorm(n_x, mean = mean_x, sd = sd_x)
  #   y <- rnorm(n_y, mean = mean_y, sd = sd_y)
  #   
  #   # Perform Lepage test
  #   test <- nonpar::lepage.test(x, y)
  #   values$p[i] <- test$p.value
  #   values$D[i] <- test$obs.stat
  # }
 return(testresults) 
}
