options(digits = 3)

data("mtcars")
fit <- lm(mpg ~ wt, mtcars)
summary(fit)
predict(fit, newdata = data.frame(x = mean(mtcart)), interval = ("confidence"))


# Multivariable regression

require(datasets); data(swiss); require(GGally); require(ggplot2)
g = ggpairs(swiss, lower = list(continuous = "smooth"),params = c(method = "loess"))
summary(lm(Fertility ~ . , data = swiss))$coefficients

summary(lm(Fertility ~ Agriculture, data = swiss))$coefficients

n <- 100; x2 <- 1 : n; x1 <- .01 * x2 + runif(n, -.1, .1); y = -x1 + x2 + rnorm(n, sd = .01)
summary(lm(y ~ x1))$coef
summary(lm(y ~ x1 + x2))$coef

dat = data.frame(y = y, x1 = x1, x2 = x2, ey = resid(lm(y ~ x2)), ex1 = resid(lm(x1 ~ x2)))
g = ggplot(dat, aes(y = y, x = x1, colour = x2))
g = g + geom_point(colour="grey50", size = 5) + geom_smooth(method = lm, se = FALSE, colour = "black") 
g = g + geom_point(size = 4) 
g