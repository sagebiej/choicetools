# function to split model into different columns for texreg

subcoef <- function(condition, mname){

  sub <- grep(condition,slot(mname,"coef.names"))

  for (ele in c("coef.names","coef","se","pvalues"))  {
    elements<- slot(mname,ele)[sub]
    slot(mname,ele) <- elements
  }

  slot(mname,"coef.names")<-gsub(pattern = condition,replacement = "",x =slot(mname,"coef.names") )

  slot(mname,"model.name")<-gsub("_","",condition)


  return(mname)

}
