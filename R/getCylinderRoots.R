#' Title
#'
#' @param mo Double. Inhomogeinity ratio. mo=0: homogeneous material. 0<=mo<=0.95
#' @param lo Double. Truncation Ratio lo<=0.5. no=0: no truncation. (bo=0)
#' @param no Integer. Number of mode n=1..8
#' @param model Character. "linear", "nonlinear", "trees"
#' @param extrapolate Logical. Admit extrapolate values?
#' @param OSF Double. Outlier scale factor
#' 
#'
#' @return Number. Cylinder roots
#' @export getCylinderRoots
#'
#' @import data.table
#' @import stats
#' @import rpart
#' @import randomForest 
#'
#' @examples
#'
getCylinderRoots <- function(no=1,mo,lo,model="nonlinear",extrapolate=TRUE,OSF=0.10){
  on.exit(expr={rm(list = ls())}, add = TRUE)
 
  OK <- exists("CylinderRoots") & !is.null(CylinderRoots)
  stopifnot(OK)
  . <- NULL
  # Check ranges
  if(!(no %in% seq(min(CylinderRoots$n), max(CylinderRoots$n)) )){
    stop(sprintf("no=%d .Wrong mode number",no))}
  DATA <- CylinderRoots[n==no,-c("n")] |> unique()
  # Check if you were lucky
  if(nrow(DATA[l==lo & m==mo])>0){
    VALUE <- DATA[l==lo & m==mo]$an
    return(VALUE)
  }
  # Check ranges
  lo_max <- max(DATA$l)
  if(lo>lo_max & extrapolate==FALSE){
    lo <- min(lo,lo_max)
  }
  
  mo_max <- max(DATA$m)
  if(mo>mo_max & extrapolate==FALSE){
    mo <- min(mo,mo_max)
  }
  
  # Linear interpolation
  if(model=="lm"){
    MODEL <- glm(an ~l+m+l2+m2,data=DATA[,.(an,l,m,l2=l^2,m2=m^2)])
    NEWDATA <- list(l=lo,m=mo,l2=lo^2,m2=mo^2)
    VALUE <- predict(MODEL,newdata=NEWDATA)
    
  }
  # Non-Linear interpolation. Mixed effects. Best 
  if(model=="nlm"){
    MODEL <- glm(an ~l+m+l2+m2+l*m+l2*m2,data=DATA[,.(an,l,m,l2=l^2,m2=m^2)])
    NEWDATA <- list(l=lo,m=mo,l2=lo^2,m2=mo^2)
    VALUE <- predict(MODEL,newdata=NEWDATA)
    
  }
  
  # Decision Trees
  if(model=="dt"){
    MODEL <- rpart(an ~ l+m,data=DATA)
    NEWDATA <- list(l=lo,m=mo)
    VALUE <- predict(MODEL,newdata=NEWDATA)
    }
  
  
  # Random Forest
  if(model=="rf"){
    # Reduce the dataset
    DATA <- DATA[ between(m,(1-OSF)*mo,(1+OSF)*mo) & between(l,(1-OSF)*lo,(1+OSF)*lo)]
    MODEL <- randomForest(an ~ l+m,data=DATA,importance=FALSE,proximity=FALSE)
    NEWDATA <- list(l=lo,m=mo)
    VALUE <- predict(MODEL,newdata=NEWDATA)
  }
  if(lo>lo_max & mo<=mo_max & extrapolate==TRUE){
    warning(sprintf("lo=%g is higher than lo_max=%g. The rooth an=%g is a predicted value that may not satisfy the boundary conditions of characteristics (cylinder) equation. Use this value with proper judgement.",lo,lo_max,VALUE))
  }
  if(mo>mo_max & lo<=lo_max & extrapolate==TRUE){
    warning(sprintf("mo=%g is higher than mo_max=%g. The root an=%g is a predicted value that may not satisfy the boundary conditions of characteristics (cylinder) equation. Use this root value with proper judgement.",mo,mo_max,VALUE))
  }
  if(mo>mo_max & lo>lo_max & extrapolate==TRUE){
    warning(sprintf("mo=%g and lo=%g  are higher than mo_max=%g and lo_max=%g, respectively. The root an=%g is a predicted value that may not satisfy the boundary conditions of characteristics (cylinder) equation. Use this root value with proper judgement.",mo,lo,mo_max,lo_max,VALUE))
  }
  return(VALUE)
}
