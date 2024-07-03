library(dplyr)
library(ggplot2)
library(e1071)
data(mpg)
head(mpg)
summary(mpg)

#1
p <- mean(mpg$manufacturer == "honda")
n <- 10000
theta <- numeric(n)
Y <- numeric(n)

for (i in 1:n) {
  theta[i] <- rbinom(1, 1, p)
  prob <- ifelse(theta[i] == 1, p, 1 - p)
  Y[i] <- rbinom(1, 1, prob)
}

mean(theta[Y == 1])



#2
successes <- sum(mpg$manufacturer == "honda")
failures <- sum(mpg$manufacturer != "honda")
n_iter <- 10000
alpha <- 1
beta <- 1
theta <- numeric(n_iter)
theta[1] <- rbeta(1, alpha + successes, beta + failures)

for (i in 2:n_iter) {
  theta[i] <- rbeta(1, alpha + successes, beta + failures)
}

hist(theta, probability = TRUE, main = "Posterior distribution of theta (MPG Compact)",
     xlab = "theta", col = "yellow", border = "black")



#3
mpg_naive <- mpg[, c("cty", "hwy", "manufacturer")]
naive_model_mpg <- naiveBayes(manufacturer ~ ., data = mpg_naive)
predictions_mpg <- predict(naive_model_mpg, mpg_naive, type = "raw")
mpg_naive$Predicted_Probability <- predictions_mpg[, "honda"]


ggplot(mpg_naive, aes(x = cty, y = hwy, color = Predicted_Probability)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Probability of Honda given City and Highway MPG",
       x = "City MPG", y = "Highway MPG")

#4
# Other features useful to predict a car manufacturer: we can use transmission and class








