fitmod <- function(data, equation, family=gaussian, gimme="diag") {
	gimme <- match.arg(gimme, choices=c("df","diag","results"))
	
	conn_mod1 <- glm(equation, family=family, data=data)
	m1 <- fortify(conn_mod1, data)
	# 	anov <- Anova(conn_mod1, type=3)
	mod <- summary(conn_mod1)
	a <- ggplot(m1, aes_string(x = as.character(equation)[[2]]) ) + geom_histogram()
	b <- qplot(.fitted, .stdresid, data=m1, geom="point")
	c <- qplot(.stdresid, data=m1, geom="histogram")
	d <- qplot(.hat, .cooksd, data=m1, geom="point")
	# 	shaptest <- shapiro.test(residuals(mod))
	
	if(gimme=="df"){
		temp <- data.frame(mod[[4]])
		temp$vars <- row.names(temp)
		temp
	} else if(gimme=="diag")
	{
		list(do.call(grid.arrange,  list(a,b,c,d)))
	} else { mod }
}