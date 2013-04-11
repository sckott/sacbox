#' Fit model tools.
#' 
#' @param data Data.frame to input
#' @param equation Model equation (e.g., y ~ x), not quoted
#' @param gimme What to return, one of df, diag, or results.
#' @examples 
#' mod1 <- lm(Sepal.Length ~ Petal.Width * Species, data=iris)
#' modfit_fixed_only(data=iris, equation=mod1, gimme="diag")
#' @export
modfit_fixed_only <- function(data, equation=NULL, gimme="diag") {  
  gimme <- match.arg(gimme, choices=c("df","diag","results","fit"))
  
  conn_mod1 <- lm(equation, data=data)
  m1 <- fortify(conn_mod1, data)
  # 	anov <- Anova(conn_mod1, type=3)
  mod <- summary(conn_mod1)
  a <- ggplot(m1, aes_string(x = as.character(equation)[[2]]) ) + geom_histogram() + ggtitle("Dist. of response variable")
  b <- qplot(.fitted, .stdresid, data=m1, geom="point") + ggtitle("Dist. of resid vs. fitted")
  c <- qplot(.stdresid, data=m1, geom="histogram") + ggtitle("Dist. of model resiudals")
  shaptest <- shapiro.test(residuals(mod))
  
  if(gimme=="df"){
    temp <- data.frame(mod[[4]])
    temp$vars <- row.names(temp)
    row.names(temp) <- NULL
    temp
  } else if(gimme=="diag")
  {
    list(do.call(grid.arrange,  list(a,b,c)), shaptest)
  } else if(gimme=="results"){ 
    temp <- data.frame(Anova(conn_mod1, type=3))
    temp$vars <- row.names(temp)
    row.names(temp) <- NULL
    temp
  } 
  else { conn_mod1 } 
}