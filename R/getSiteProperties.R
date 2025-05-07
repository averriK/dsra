#' Title
#'
#' @param Hs Double. 
#' @param POP Integer. 
#' @param Water Double.
#' @param NR Integer
#' @param h Double
#' @param levels list
#' @param USCS Character.
#'
#' @return List
#' @export getSiteProperties
#'
#' @import data.table
#' @importFrom stats predict
#'


getSiteProperties<- function(Hs,USCS,POP=100,Water=0,NR=1,h=1.00,levels=c(0.05,0.5,"mean",0.95)){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  . <- NULL
  
  # Build dataset
  SiteTable <- data.table()
  
  
  for(pop in POP){
    for(hs in Hs){
      for(w in Water){
        for(i in 1:NR){
          cat(sprintf("\rBuilding sample %d/%d..",i,NR))
          DT <- geSiteTable(Hs=hs,h=h,Water=w,USCS=USCS,POP=pop)
          SiteTable <- rbindlist(list(DT,SiteTable),use.names = TRUE)
        }
      }
    }
  }
  cat("Done!\n")
 
  
  DT <- data.table()
  if(any(levels=="mean")){
    DT <- SiteTable[,.(
      USCS=paste0(USCS,collapse=" "),
      Go=mean(Go)|> round(1),
      mo=mean(mo)|> round(3),
      Ts=mean(Ts)|> round(2),
      VSo=mean(VSo) |> round(1),
      VS30=mean(VS30) |> round(1),
      Z500=mean(Z500) |> round(1),
      Z1000=mean(Z1000),
      level="mean"),by=.(Hs,POP,Hw)] |> rbind(DT)
   
  }
  if(any(levels!="mean")){
    probs <- levels[levels!="mean"] |> as.numeric()
    DT <- SiteTable[,.(
      USCS=paste0(USCS,collapse=" "),
      Go=quantile(Go,probs = probs)|> round(1),
      mo=quantile(mo,probs = probs) |> round(3),
      Ts=quantile(Ts,probs = probs)|> round(2),
      VSo=quantile(VSo,probs = probs) |> round(1),
      VS30=quantile(VS30,probs = probs) |> round(1),
      Z500=quantile(Z500,probs=probs) |> round(1),
      Z1000=quantile(Z1000,probs=probs) |> round(1),
      level=probs),by=.(Hs,POP,Hw)]  |> rbind(DT)
    
  }
  return(DT)
    
  
  
}

