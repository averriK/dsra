#' Title
#'
#' @param Hs Double. 
#' @param POP Integer. 
#' @param Water Double.
#' @param .newdata Data frame. New data to predict
#' @param NIT Integer
#' @param NR Integer
#'
#' @return List
#' @export geSiteTable
#'
#' @import data.table
#' @import xstats
#' @import quantregForest
#' @importFrom stats predict
#'
#' @examples
#'

geSiteTable <- function(Hs,USCS,POP=100,Water=0,NR=1,h=1.00,levels=c(0.05,0.5,"mean",0.95)){
  on.exit(expr={rm(list = ls())}, add = TRUE)
  . <- NULL
  
  # Build dataset
  SiteTable <- data.table()
  
  
  for(pop in POP){
    for(hs in Hs){
      for(w in Water){
        for(i in 1:NR){
          cat(sprintf("\rBuilding sample %d/%d..",i,NR))
          DT <- buildDSRA(Hs=hs,h=h,Water=w,USCS=USCS,POP=pop)
          SiteTable <- rbindlist(list(DT,SiteTable),use.names = TRUE)
        }
      }
    }
  }
 
  
  DT <- data.table()
  if(any(levels=="mean")){
    DT <- SiteTable[,.(
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
    
  
  # Go
  # COLS <- c(names(.newdata),"Go")
  # DATA <- SiteTable[,..COLS] |> unique()
  # .model <- randomForest::randomForest(Go ~ .,data=DATA,importance=FALSE,proximity=FALSE)
  # Go <- predict(.model,newdata=.newdata) |> round(1)
  # #
  # # mo
  # COLS <- c(names(.newdata),"mo")
  # DATA <- .data[,..COLS] |> unique()
  # .model <- randomForest::randomForest(mo ~ .,data=DATA,importance=FALSE,proximity=FALSE)
  # mo <- stats::predict(.model,newdata=.newdata) |> round(3)
  # 
  # # Ts
  # COLS <- c(names(.newdata),"Ts")
  # DATA <- .data[,..COLS] |> unique()
  # .model <- randomForest::randomForest(Ts ~ .,data=DATA,importance=FALSE,proximity=FALSE)
  # Ts <- stats::predict(.model,newdata=.newdata) |> round(2)
  # 
  # # VSo
  # COLS <- c(names(.newdata),"VSo")
  # DATA <- .data[,..COLS] |> unique()
  # .model <- randomForest::randomForest(VSo ~ .,data=DATA,importance=FALSE,proximity=FALSE)
  # VSo <- stats::predict(.model,newdata=.newdata) |> round(0)
  # 
  # # VS30
  # COLS <- c(names(.newdata),"VS30")
  # DATA <- .data[,..COLS] |> unique()
  # .model <- randomForest::randomForest(VS30 ~ .,data=DATA,importance=FALSE,proximity=FALSE)
  # VS30 <- stats::predict(.model,newdata=.newdata) |> round(0)
  
  # return(data.table(Go=Go,mo=mo,Ts=Ts,VSo=VSo,VS30=VS30))
  
}

# getShearParameters <- function(.data=SiteTable,.newdata,level="mean",regression="qrf"){
#   on.exit(expr={rm(list = ls())}, add = TRUE)
#   . <- NULL
#   #   # If Hs is prescribed, filter data an remove parameter
#     if("Hs" %in% names(.newdata)){
#       SET <- .data[Hs %in%  unique(.newdata$Hs)] |> unique()
#       if(nrow(SET)>100){
#         .data <- SET
#       }
#       # .newdata <- .newdata[,-c("Hs")] |> unique()
#     }
# 
#   if("Water" %in% names(.newdata)){
#     SET <- .data[Water %in%  unique(.newdata$Water)] |> unique()
#     if(nrow(SET)>100){
#       .data <- SET
#     }
#     # .newdata <- .newdata[,-c("Hs")] |> unique()
#   }
# 
# 
#   if("POP" %in% names(.newdata)){
#     SET <- .data[POP %in% unique(.newdata$POP)] |> unique()
#     if(nrow(SET)>100){
#      .data <- SET
#     }
#     # .newdata <- .newdata[,-c("Hs")] |> unique()
#   }
#   # Go
#   COLS <- c(names(.newdata),"Go")
#   DATA <- .data[,..COLS] |> unique()
#   .model <- randomForest::randomForest(Go ~ .,data=DATA,importance=FALSE,proximity=FALSE)
#   Go <- predict(.model,newdata=.newdata) |> round(1)
# #
#   # mo
#   COLS <- c(names(.newdata),"mo")
#   DATA <- .data[,..COLS] |> unique()
#   .model <- randomForest::randomForest(mo ~ .,data=DATA,importance=FALSE,proximity=FALSE)
#   mo <- stats::predict(.model,newdata=.newdata) |> round(3)
# 
#   # Ts
#   COLS <- c(names(.newdata),"Ts")
#   DATA <- .data[,..COLS] |> unique()
#   .model <- randomForest::randomForest(Ts ~ .,data=DATA,importance=FALSE,proximity=FALSE)
#   Ts <- stats::predict(.model,newdata=.newdata) |> round(2)
# 
#   # VSo
#   COLS <- c(names(.newdata),"VSo")
#   DATA <- .data[,..COLS] |> unique()
#   .model <- randomForest::randomForest(VSo ~ .,data=DATA,importance=FALSE,proximity=FALSE)
#   VSo <- stats::predict(.model,newdata=.newdata) |> round(0)
# 
#   # VS30
#   COLS <- c(names(.newdata),"VS30")
#   DATA <- .data[,..COLS] |> unique()
#   .model <- randomForest::randomForest(VS30 ~ .,data=DATA,importance=FALSE,proximity=FALSE)
#   VS30 <- stats::predict(.model,newdata=.newdata) |> round(0)
# 
#   return(data.table(Go=Go,mo=mo,Ts=Ts,VSo=VSo,VS30=VS30))
# 
# }

# fitModel.Shear <- function(.data=SiteTable,.newdata,level="mean",regression="qrf"){
#   on.exit(expr={rm(list = ls())}, add = TRUE)
#   . <- NULL
#   DATA <- .data
#   NEWDATA <- .newdata
#   # If Hs is prescribed, filter data an remove parameter
#   if("Hs" %in% names(.newdata)){
#     Hso <- .find(V=unique(.data$Hs) , X=unique(.newdata$Hs))
#     DATA <- DATA[Hs %in% Hso]
#     NEWDATA <- NEWDATA[,-c("Hs")]
#   }
#
#   if("Gravels" %in% names(.newdata) & all(.newdata$Gravels %in% c(0,100))){
#     DATA <- DATA[Gravels %in% .newdata$Gravels]
#     NEWDATA <- NEWDATA[,-c("Gravels")]
#   }
#
#   if("Sands" %in% names(.newdata) & all(.newdata$Sands %in% c(0,100))){
#     DATA <- DATA[Sands %in% .newdata$Sands]
#     NEWDATA <- NEWDATA[,-c("Sands")]
#   }
#
#   if("Clays" %in% names(.newdata) & all(.newdata$Clays %in% c(0,100))){
#     DATA <- DATA[Clays %in% .newdata$Clays]
#     NEWDATA <- NEWDATA[,-c("Clays")]
#   }
#
#   if("Silts" %in% names(.newdata) & all(.newdata$Silts %in% c(0,100))){
#     DATA <- DATA[Silts %in% .newdata$Silts]
#     NEWDATA <- NEWDATA[,-c("Silts")]
#   }
#
#   if("Water" %in% names(.newdata) & all(.newdata$Water %in% c(0,100))){
#     DATA <- DATA[Water %in% .newdata$Water]
#     NEWDATA <- NEWDATA[,-c("Water")]
#   }
#   NR <- nrow(DATA)
#
#   if(NR==0){
#     message("No data available. Stop")
#     return(NULL)
#   }
#
#   Go <- fitModel(.data = DATA, response="Go",.newdata=NEWDATA,level=level,regression=regression) |> round(1)
#   mo <- fitModel(.data = DATA, response="mo",.newdata=NEWDATA,level=level,regression=regression) |> round(3)
#   Ts <- fitModel(.data = DATA, response="Ts",.newdata=NEWDATA,level=level,regression=regression) |> round(2)
#   VSo <- fitModel(.data = DATA, response="VSo",.newdata=NEWDATA,level=level,regression=regression) |> round(0)
#   VS30 <- fitModel(.data = DATA, response="VS30",.newdata=NEWDATA,level=level,regression=regression) |> round(0)
#   return(data.table(Go=Go,mo=mo,Ts=Ts,VSo=VSo,VS30=VS30,NEWDATA,NR=NR))
#
# }
