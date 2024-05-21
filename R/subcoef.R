# function to split model into different columns for texreg

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
