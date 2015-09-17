#' Documentation
#' 
library(devtools)
library(datasets)
library(stats)
library(ggplot2)
data(iris)
attach(iris)
View(iris)
?iris
lm(iris)

linreg <- function(formula, data){
  
  X <- model.matrix(formula, data=data)
  y <- all.vars(formula) ####### problem... 
  
  # Regressions coefficients
  bhat <- solve(t(X)%*%X) %*% t(X) %*% y
  # Fitted values
  yhat <- X%*%bhat
  # residuals
  resid <- y - yhat
  # Degrees of freedom
  n <- length(iris)
  p <- length (bhat)
  df <- n-p
  # Residual variance
  sigma2 <- (t(x)%*%e)/df
  # variance of the regression coefficients
  varcoeff <- sigma2 * solve(t(X)%*%X)
  # t values
  tstat <- betahat / sqrt(varcoeff)
  # p-value
  p <- pt(betahat)
  
  reg <- as.linreg(c(bhat, yhat, resid, n, p, df, sigma2, varcoeff, tstat, p)) # ?
  return(reg)
}

f1 <- Petal.Length ~ Species
linreg(formula=f1, data=iris) # by the moment it doesnt work...

mod_object <- lm(Petal.Length~Species, data=iris)
print(mod_object)
plot(mod_object)
resid(mod_object)
predict(mod_object) # only pred doesnt work
coef(mod_object)
summary(mod_object)
