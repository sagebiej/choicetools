## This function is used to create WTP values using the delta method.


wtp <- function(cost, attr, modelname, mediancost=FALSE) {

  wtp_values =data.frame(wtp =numeric(), robse=numeric() , robt= numeric() )
  attr <- attr[-which(attr==cost)]


  if (mediancost) {cost=paste0("-exp(",cost,")")

  }

  for (a in attr) {

    ex <- paste0(a,"/",cost)

    deltaMethod_settings=list(expression=(temp=ex))
    #deltaMethod_settings=list(operation="ratio", parName1=a, parName2=cost)
    wtp_values[which(attr==a),]<- apollo_deltaMethod(modelname, deltaMethod_settings)[,2:4]
  }

  #names(wtp_values) <- c("Expression" , "wtp" , "robse" , "robt")
  wtp_values$wtp <- wtp_values$wtp*-1
  wtp_values$robse <- wtp_values$robse*1
  wtp_values$robt <- wtp_values$robt*-1
  wtp_values$pVal <- (1-pnorm((abs(wtp_values$robt))))*2

  rownames(wtp_values) <- attr
  return(wtp_values)

}
