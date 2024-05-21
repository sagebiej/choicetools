#this function uses the wtp function and wraps it for all Classes in a LC model.

wtp_lc <-function(modelname) {


  wtpvalues=list()

  for (class in 1:(length(modelname$LL0)-1)) {


    clet <- intToUtf8(96+class)
    coefs<- data.frame(apollo::apollo_modelOutput(modelname, modelOutput_settings = list(printPVal=T)))
    coefs<- coefs[grep(paste0("delta_.*", clet,"$"),x = rownames(coefs), value=TRUE, perl = TRUE), c(1,5:7)]




    print(clet)

    wtpvalues[[paste0("Class ",class)]] <-
      wtp(paste0("cost_",clet),
          grep(paste0("^(?=.*_",clet,")(?!.*delta)"), names(modelname$estimate) , value=T, perl = TRUE),modelname = modelname)

    colnames(coefs)<- colnames(wtpvalues[[paste0("Class ",class)]])

    wtpvalues[[paste0("Class ",class)]] <- rbind(wtpvalues[[paste0("Class ",class)]],coefs)

    #  print("this is other")
    #  print(other)
    #  if(class(other) =="numeric") other<-as.data.frame(as.list(other))
    # colnames(other) <- names(wtpvalues[[paste0("Class ",class)]])
    # print("this is other after rename")
    # print(other)
    #  wtpvalues[[paste0("Class ",class)]] <-rbind(wtpvalues[[paste0("Class ",class)]],other)
  }

  return(wtpvalues)

}
