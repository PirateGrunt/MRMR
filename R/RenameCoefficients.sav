#' @title RenameCoefficients
#' 
#' @description
#' This function will return all of the values for the most recent evaluation date. Note 
#' that this applies for each origin period individually. For example, if some origin periods have an 
#' evaluation at December 31, 2010, but others only have evaluations at December 31, 2009, the data frame 
#' which is returned will have two different evaluation dates present.
#' 
#' @param
#' x a data frame or a triangle
#' 
#' @return
#' A data frame
#' 
#'  
RenameCoefficients = function(Fit, Names){
  coef = Fit$coefficients
  names(coef) = Names
  Fit$coefficients = coef
  
  Fit
}

# @example
#  x = 1:50
#  y = 5 + 3*x + rnorm(50)
#  fit = lm(y~x)
#  summary(fit)
#  fit = RenameCoefficients(fit, c("Intercept", "Slope"))
#  summary(fit)