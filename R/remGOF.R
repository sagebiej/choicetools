remGOF<- function(models){

  gof<- function(m){
    methods::slot(m,"gof.names")<- character(0)
    methods::slot(m,"gof")<- numeric(0)
    methods::slot(m,"gof.decimal")<- logical(0)

    return(m)
  }

  return(purrr::map(models,gof))

}
