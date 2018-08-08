library(dplyr)

## Standardization
variables_0 <- variables %>% mutate(SEATBELT = 0, DEATH = NA)
variables_1 <- variables %>% mutate(SEATBELT = 1, DEATH = NA)

reg <- glm(DEATH ~ (DRIVER*AGE**2 + SEX + AGE**2)*SEATBELT + SEATBELT + (DRIVER*AGE**2 + SEX + AGE**2), family = "binomial", data = variables)

pred_0 <- predict.glm(reg, variables_0, type="response")
variables_0$DEATH = pred_0

pred_1 <- predict.glm(reg, variables_1, type="response")
variables_1$DEATH = pred_1

std_mu_0 <- mean(pred_0)
std_mu_1 <- mean(pred_1)

## Bootstrapping
std <- data_frame(std_mu_0, std_mu_1)

for(i in 1:1000){
  bootstrap <- sample_n(variables, nrow(variables), replace=TRUE)
  bootstrap_0 <- bootstrap %>% mutate(SEATBELT = 0, DEATH = NA)
  bootstrap_1 <- bootstrap %>% mutate(SEATBELT = 1, DEATH = NA)
  
  pred_0 <- predict.glm(reg, bootstrap_0, type="response")
  pred_1 <- predict.glm(reg, bootstrap_1, type="response")
  
  std <- rbind(std, c(mean(pred_0), mean(pred_1)))
  
  if(i %% 100 == 0){
    cat("Sample", i, "\n")
  }
}

std_mu_0 = mean(std$std_mu_0)
std_mu_1 = mean(std$std_mu_1)








