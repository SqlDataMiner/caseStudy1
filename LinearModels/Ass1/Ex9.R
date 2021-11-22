x <- (1:12)
y <- c(283, 339, 344, 327, 330, 355, 323, 301, 305, 266, 256, 203)

beet_data <- data.frame(x, y)
beet_data$x

linear_only_model <- lm(beet_data$y ~ beet_data$x)
quadratic_model <- lm(beet_data$y ~ beet_data$x + I(beet_data$x^2))
cubic_model <- lm(beet_data$y ~ beet_data$x + I(beet_data$x^2) + I(beet_data$x^3))

plot(beet_data$x, beet_data$y,
     main="log dose vs harvest weight of sugar beet",
     xlab="log dose(x)",
     ylab="weight of sugar beet obtained at harvest")
abline(linear_only_model, col="green")
lines(beet_data$x, quadratic_model$fitted, col="red" )
lines(beet_data$x, cubic_model$fitted, col="blue")
points(beet_data$x, linear_only_model$fitted, pch=4)
points(beet_data$x, quadratic_model$fitted, pch=5)
points(beet_data$x, cubic_model$fitted, pch=6)
legend("bottomleft",c("linear terms only","linear and quadratic terms","linear, quadratic and cubic terms"),
       col=c("deepskyblue4","red","green"), lwd=3)

summary(linear_only_model)
summary(quadratic_model)
summary(cubic_model)