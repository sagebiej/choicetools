
#z-test with apollo data

apollo_ztest <- function(model1, model2, hyp=0){

  comp = data.frame(m1par =model1[["estimate"]] ,m2par = model2[["estimate"]] , m1se=model1[["robse"]] , m2se=model2[["robse"]]) %>%

    mutate(diffmean=m1par-m2par , error= sqrt(m1se^2+m2se^2) , z= diffmean/error , p_value=2*pnorm(-abs(z)))


  print(comp)

  cat(class(comp))

  return(comp)

}
