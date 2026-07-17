# function for predicting effects

Predict_effects <- function(Newdata, Model, rescale, Cont_or_Cat, seMultiplier, LU_n, BT) {

# function for backtransforming  
  
  logit2prob <- function(logit){
  return(exp(logit)/(1+exp(logit)))
  }
  
# which coefs are not estimated?  

coefs <- mvrnorm(n=1, mu=fixef(Model), Sigma=vcov(Model))
mm <- model.matrix(terms(Model), Newdata)
print(setdiff(names(coefs), colnames(mm)))

# predictions, for categories  
if(Cont_or_Cat=="categorical"){
preds <- sapply(X=1:1000, FUN=function(i){
  coefs <- mvrnorm(n=1, mu=fixef(Model), Sigma=vcov(Model))
  mm <- model.matrix(terms(Model), Newdata)

  ## drop coefs that couldn't be estimated      
  
  print(setdiff(colnames(mm), names(coefs)))
  to_drop <- setdiff(colnames(mm), names(coefs))
  if(length(to_drop)!=0){
    mm <- as.data.frame(mm)
    mm <- mm[, -which(colnames(mm) %in% to_drop)]
    mm <- as.matrix(mm)
  }
  
  y <- mm %*% coefs
  
  # backtranforming      
  if(BT){
  y <- logit2prob(y)
}
# rescaling      
  if(rescale){
# initialisation        
    seq <- 1:LU_n        y[seq] <- y[seq]/y[seq[1]]*100        
    
    # for loop to rescale all values        
    for(i in 1:(nrow(Newdata)/LU_n-1)){
seq <- seq + LU_n          y[seq] <- y[seq]/y[seq[1]]*100        }
}
return(y)
})
preds <- data.frame(Median=apply(X=preds, MARGIN=1, FUN=median),
                    Upper=apply(X=preds, MARGIN=1, FUN=quantile, probs=0.975),
                    Lower=apply(X=preds, MARGIN=1, FUN=quantile, probs=0.025))
preds <- cbind(preds, Newdata)
return(preds)
}
if(Cont_or_Cat=="continuous"){
  mm <- model.matrix(terms(Model), Newdata)
  # predictions    
  preds <- mm %*% fixef(Model)
  # backtransform    
  Newdata$Estimate <- logit2prob(preds)
  # getting the errors around the predicted values    
  # variance covariance matrix and matrix multiplication    
  VarCoVar <- as.matrix(vcov(Model))
  VarCoVar <- base::tcrossprod(VarCoVar, mm)
  # matrix multiplication to get estimates and take diagonal matrix    
  pvar <- diag(mm %*% VarCoVar)
  # estimate error of predictions and backtransform    
  Lower <- preds - seMultiplier*sqrt(pvar)
  Upper <- preds + seMultiplier*sqrt(pvar)
  Newdata$Lower <- logit2prob(Lower)
  Newdata$Upper <- logit2prob(Upper)
  return(Newdata)
}
}