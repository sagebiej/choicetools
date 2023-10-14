remGOF<- function(models){

  gof<- function(m){
    slot(m,"gof.names")<- character(0)
    slot(m,"gof")<- numeric(0)
    slot(m,"gof.decimal")<- logical(0)

    return(m)
  }

  return(purrr::map(models,gof))

}
