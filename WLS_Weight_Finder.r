# weight finder for Weighted Least Squares

weight_finder <- function(formulah, data, wght = NULL){
        regression <- lm(formula(formulah) ,data=data, weights = wght)
                resid <- regression$residuals #storing residuals from REG_0
                pr <- predict(regression) # storing predicted values
                smooth.var <- loess(resid^2 ~ pr) # fitting nonparametric regression 
                vars <- predict(smooth.var) # storing predicted values from loess regression
return(1/vars)
}
