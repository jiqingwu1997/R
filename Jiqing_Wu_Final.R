##
## Jiqing Wu
##

##
## Problem 1
##

load("/Users/jiqingwu/Desktop/final.RData")
stackDataInList<-function(alist){
  a<-data.frame()
  for(i in 1:length(alist)){
  b<-alist[[i]]
  a<-rbind(a,b)
  }
  return(a)
}
stackDataInList(datList[1])
stackDataInList(datList[c(1,3,4)])
stackDataInList(datList)


##
## Problem 2
##

impute<-function(...,dat){
  datList<-c(...)
  if (length(datList>0)){
  result<-data.frame()
  for (i in 1:length(datList)){
    if (is.numeric(dat[[datList[i]]])==T){
      dat[[datList[i]]][which(is.na(dat[[datList[i]]]))]<-median(dat[[datList[i]]],na.rm=T)
      }else{
      dat[[datList[i]]][which(is.na(dat[[datList[i]]]))]<-names(which(table(dat[[datList[i]]])==max(table(dat[[datList[i]]]))))
      }
    result1=as.data.frame(dat[[datList[i]]])
    colnames(result1)=datList[i]
    result<-as.data.frame(c(result,result1))
   }
  }else{
    result<-data.frame()
    for (i in 1:length(dat)){
      if (is.numeric(dat[[i]])==T){
        dat[[i]][which(is.na(dat[[i]]))]<-median(dat[[i]],na.rm=T)
      }else{
        dat[[i]][which(is.na(dat[[i]]))]<-names(which(table(dat[[i]])==max(table(dat[[i]]))))
      }
      result1=as.data.frame(dat[[i]])
      colnames(result1)=names(dat[i])
      result<-as.data.frame(c(result,result1))
    }
  }
  return(result)
}
impute (dat=patient)
impute ("LDL", "HRT", "MAMM", dat=patient)
impute("ID", "GLUC", "TGL", "HDL", dat=patient)


##
## Problem 3
##

myCortest<-function(...,mainVar,dat){
  Varlist=c(...)
  r=c()
  p=c()
  for (i in 1:length(Varlist)){
    rValue=as.numeric(cor.test(dat[[mainVar]],dat[[Varlist[i]]])$estimate)
    pValue=as.numeric(cor.test(dat[[mainVar]],dat[[Varlist[i]]])$p.value)
    r=rbind(r,rValue)
    p=rbind(p,pValue)
  }
  result<-data.frame(
    var1=rep(mainVar,length(Varlist)),
    var2=Varlist,
    R=r,
    p=p)
  rownames(result)<-Varlist
  return(result)
}
myCortest ("age", mainVar="wt", dat=chol)
myCortest ("age", "chol", "tg", "ht", mainVar="wt", dat=chol)
myCortest ("sbp", "dbp", "vldl", "hdl", "ldl", mainVar="bmi", dat=chol)

##
## Problem 4
##

confound.all<-function(...,dat,outcome,p){
  mainVar = c(...)
  VarList = c(...)
  
  Cov = sapply(mainVar, function(x){
    mainCoeff = lm(paste(outcome, x, sep="~"), data=dat)$coefficients[2]
   
    tempS = sapply(VarList, function(one){
      if(x == one){
        tempC = c(mainCoeff = as.vector(-1000), 
                  CovariateCoeff = as.vector(-1000), 
                  percentChange = as.vector(-1000), 
                  confounder = as.vector(-1000))
       
      }else{
        CovCoeff=lm(paste(outcome, "~", x, "+", one),data=dat)$coefficients[2]
        percentChange=(CovCoeff-mainCoeff)/mainCoeff
        confounder=abs(percentChange)>p
        
        tempC = c(mainCoeff = as.vector(mainCoeff), 
                  CovariateCoeff = as.vector(CovCoeff), 
                  percentChange = as.vector(percentChange), 
                  confounder = as.vector(confounder))
        return(tempC)
      }
    })
    return(tempS)
  })
  
  ConfName = rep(VarList,(length(VarList)))
  A = ConfName != rep(VarList,each=(length(VarList)))
  R = data.frame(mainVar = rep(VarList,each=(length(VarList)-1)), 
                 covariate = ConfName[A],
                 Cov)
  return(R)
}
confound.all("vldl", "hdl", "ldl", dat = chol, outcome ="log(tg)", p=0.15)
