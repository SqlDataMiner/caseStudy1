library(stringr)

tractor_data <- read.table("tractor data.txt", header = TRUE)

tractor_linear_model <- lm(tractor_data$Maint ~ tractor_data$Age)
tractor_log_model <- lm(tractor_data$Maint ~ tractor_data$Age + I(log(tractor_data$Age)))

writeLines("Test H_0: Beta_1 = Beta_2 = 0")
anova(tractor_log_model)
f_b1_equals_b2_equals_zero <- ((1099635 + 23240)/2)/(1182167/14)
str_glue("The F value for beta one equals beta two equals zero is: {f_b1_equals_b2_equals_zero}")
p_b1_equals_b2_equals_zero <- 1 - pf(f_b1_equals_b2_equals_zero, 2, 14)
str_glue("The probability that beta one equals beta two equals zero is: {p_b1_equals_b2_equals_zero}")

writeLines("\n\n")

writeLines("Test H_0: Beta_2 = 0")
anova(tractor_linear_model, tractor_log_model)
writeLines("\n\n")

writeLines("Test H_0: Beta_2 = 3")
summary(tractor_log_model)
C <- rbind(c(0, 0, 1))
X <- cbind(rep(1, length(tractor_data$Age)), tractor_data$Age, I(log(tractor_data$Age)))

#values obtained from tractor_log_model$coefficients
beta_hat <- c(436.6988,  45.6270, 196.2627)


c <-c(3)
CB_less_c <- C%*%beta_hat - c
D <- solve(t(X)%*%X)
G <- solve(C%*%D%*%t(C))

f_b2_equals_3 <- (1/(2*290.6))*t(CB_less_c)%*%G%*%CB_less_c

str_glue("The F value that beta two equals three is: {f_b2_equals_3}")
p_b2_equals_three <- 1 - pf(f_b2_equals_3, 2, 14)
str_glue("The probability that beta two equals three is: {p_b2_equals_three}")

writeLines("\n\n")


writeLines("Test H_0: Beta_1 = Beta_2")
C <- rbind(c(0, 1, -1))
X <- cbind(rep(1, length(tractor_data$Age)), tractor_data$Age, I(log(tractor_data$Age)))

#values obtained from tractor_log_model$coefficients
beta_hat <- c(436.6988,  45.6270, 196.2627)


c <-c(0)
CB_less_c <- C%*%beta_hat - c
D <- solve(t(X)%*%X)
G <- solve(C%*%D%*%t(C))

f_b2_equals_b1 <- (1/(2*290.6))*t(CB_less_c)%*%G%*%CB_less_c

str_glue("The F value that beta two equals beta_1 is: {f_b2_equals_b1}")
p_b2_equals_b1 <- 1 - pf(f_b2_equals_b1, 2, 14)
str_glue("The probability that beta two equals beta one is: {p_b2_equals_b1}")

