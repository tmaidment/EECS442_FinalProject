library(dplyr)

## Standardization
variables_0 <- variables %>% mutate(REST_USE = factor(0), DEATH = NA)
variables_1 <- variables %>% mutate(REST_USE = factor(1), DEATH = NA)
variables_2 <- variables %>% mutate(REST_USE = factor(2), DEATH = NA)
variables_3 <- variables %>% mutate(REST_USE = factor(3), DEATH = NA)
variables_4 <- variables %>% mutate(REST_USE = factor(4), DEATH = NA)
variables_5 <- variables %>% mutate(REST_USE = factor(5), DEATH = NA)
variables_7 <- variables %>% mutate(REST_USE = factor(7), DEATH = NA)
variables_8 <- variables %>% mutate(REST_USE = factor(8), DEATH = NA)
variables_10 <- variables %>% mutate(REST_USE = factor(10), DEATH = NA)
variables_11 <- variables %>% mutate(REST_USE = factor(11), DEATH = NA)
variables_12 <- variables %>% mutate(REST_USE = factor(12), DEATH = NA)
variables_16 <- variables %>% mutate(REST_USE = factor(16), DEATH = NA)
variables_17 <- variables %>% mutate(REST_USE = factor(17), DEATH = NA)
variables_19 <- variables %>% mutate(REST_USE = factor(19), DEATH = NA)
variables_29 <- variables %>% mutate(REST_USE = factor(29), DEATH = NA)
variables_97 <- variables %>% mutate(REST_USE = factor(97), DEATH = NA)
variables_98 <- variables %>% mutate(REST_USE = factor(98), DEATH = NA)
variables_99 <- variables %>% mutate(REST_USE = factor(99), DEATH = NA)

reg <- glm(DEATH ~ (DRIVER**2 + SEX + ns(AGE, knots=4))*REST_USE + REST_USE + (DRIVER**2 + SEX + ns(AGE, knots=4)), family = "binomial", data = variables)

pred_0 <- predict.glm(reg, variables_0, type="response")
variables_0$DEATH = pred_0
pred_1 <- predict.glm(reg, variables_1, type="response")
variables_1$DEATH = pred_1
pred_2 <- predict.glm(reg, variables_2, type="response")
variables_2$DEATH = pred_2
pred_3 <- predict.glm(reg, variables_3, type="response")
variables_3$DEATH = pred_3
pred_4 <- predict.glm(reg, variables_4, type="response")
variables_4$DEATH = pred_4
pred_5 <- predict.glm(reg, variables_5, type="response")
variables_5$DEATH = pred_5
pred_7 <- predict.glm(reg, variables_7, type="response")
variables_7$DEATH = pred_7
pred_8 <- predict.glm(reg, variables_8, type="response")
variables_8$DEATH = pred_8
pred_10 <- predict.glm(reg, variables_10, type="response")
variables_10$DEATH = pred_10
pred_11 <- predict.glm(reg, variables_11, type="response")
variables_11$DEATH = pred_11
pred_16 <- predict.glm(reg, variables_16, type="response")
variables_16$DEATH = pred_16
pred_17 <- predict.glm(reg, variables_17, type="response")
variables_17$DEATH = pred_17
pred_19 <- predict.glm(reg, variables_19, type="response")
variables_19$DEATH = pred_19
pred_29 <- predict.glm(reg, variables_29, type="response")
variables_29$DEATH = pred_29
pred_97 <- predict.glm(reg, variables_97, type="response")
variables_97$DEATH = pred_97
pred_98 <- predict.glm(reg, variables_98, type="response")
variables_98$DEATH = pred_98
pred_99 <- predict.glm(reg, variables_99, type="response")
variables_99$DEATH = pred_99

std_mu_0 <- mean(pred_0)
std_mu_1 <- mean(pred_1)
std_mu_2 <- mean(pred_2)
std_mu_3 <- mean(pred_3)
std_mu_4 <- mean(pred_4)
std_mu_5 <- mean(pred_5)
std_mu_7 <- mean(pred_7)
std_mu_8 <- mean(pred_8)
std_mu_10 <- mean(pred_10)
std_mu_11 <- mean(pred_11)
std_mu_16 <- mean(pred_16)
std_mu_17 <- mean(pred_17)
std_mu_19 <- mean(pred_19)
std_mu_29 <- mean(pred_29)
std_mu_97 <- mean(pred_97)
std_mu_98 <- mean(pred_98)
std_mu_99 <- mean(pred_99)

## Bootstrapping
std <- data_frame(std_mu_0, std_mu_1, std_mu_2, std_mu_3, std_mu_4, std_mu_5, std_mu_7, std_mu_8,
                  std_mu_10, std_mu_11, std_mu_16, std_mu_17, std_mu_19, std_mu_29, std_mu_97, std_mu_98, std_mu_99)

for(i in 1:1000){
  bootstrap <- sample_n(variables, nrow(variables), replace=TRUE)
  bootstrap_0 <- bootstrap %>% mutate(REST_USE = factor(0), DEATH = NA)
  bootstrap_1 <- bootstrap %>% mutate(REST_USE = factor(1), DEATH = NA)
  bootstrap_2 <- bootstrap %>% mutate(REST_USE = factor(2), DEATH = NA)
  bootstrap_3 <- bootstrap %>% mutate(REST_USE = factor(3), DEATH = NA)
  bootstrap_4 <- bootstrap %>% mutate(REST_USE = factor(4), DEATH = NA)
  bootstrap_5 <- bootstrap %>% mutate(REST_USE = factor(5), DEATH = NA)
  bootstrap_7 <- bootstrap %>% mutate(REST_USE = factor(7), DEATH = NA)
  bootstrap_8 <- bootstrap %>% mutate(REST_USE = factor(8), DEATH = NA)
  bootstrap_10 <- bootstrap %>% mutate(REST_USE = factor(10), DEATH = NA)
  bootstrap_11 <- bootstrap %>% mutate(REST_USE = factor(11), DEATH = NA)
  bootstrap_12 <- bootstrap %>% mutate(REST_USE = factor(12), DEATH = NA)
  bootstrap_16 <- bootstrap %>% mutate(REST_USE = factor(16), DEATH = NA)
  bootstrap_17 <- bootstrap %>% mutate(REST_USE = factor(17), DEATH = NA)
  bootstrap_19 <- bootstrap %>% mutate(REST_USE = factor(19), DEATH = NA)
  bootstrap_29 <- bootstrap %>% mutate(REST_USE = factor(29), DEATH = NA)
  bootstrap_97 <- bootstrap %>% mutate(REST_USE = factor(97), DEATH = NA)
  bootstrap_98 <- bootstrap %>% mutate(REST_USE = factor(98), DEATH = NA)
  bootstrap_99 <- bootstrap %>% mutate(REST_USE = factor(99), DEATH = NA)
  
  pred_0 <- predict.glm(reg, bootstrap_0, type="response")
  pred_1 <- predict.glm(reg, bootstrap_1, type="response")
  pred_2 <- predict.glm(reg, bootstrap_2, type="response")
  pred_3 <- predict.glm(reg, bootstrap_3, type="response")
  pred_4 <- predict.glm(reg, bootstrap_4, type="response")
  pred_5 <- predict.glm(reg, bootstrap_5, type="response")
  pred_7 <- predict.glm(reg, bootstrap_7, type="response")
  pred_8 <- predict.glm(reg, bootstrap_8, type="response")
  pred_10 <- predict.glm(reg, bootstrap_10, type="response")
  pred_11 <- predict.glm(reg, bootstrap_11, type="response")
  pred_16 <- predict.glm(reg, bootstrap_16, type="response")
  pred_17 <- predict.glm(reg, bootstrap_17, type="response")
  pred_19 <- predict.glm(reg, bootstrap_19, type="response")
  pred_29 <- predict.glm(reg, bootstrap_29, type="response")
  pred_97 <- predict.glm(reg, bootstrap_97, type="response")
  pred_98 <- predict.glm(reg, bootstrap_98, type="response")
  pred_99 <- predict.glm(reg, bootstrap_99, type="response")
  
  std <- rbind(std, c(mean(pred_0), mean(pred_1), mean(pred_2), mean(pred_3), mean(pred_4), mean(pred_5), mean(pred_7),
                      mean(pred_8), mean(pred_10), mean(pred_11), mean(pred_16), mean(pred_17), mean(pred_19),
                      mean(pred_29), mean(pred_97), mean(pred_98), mean(pred_99)))
  
  if(i %% 100 == 0){
    cat("Sample", i, "\n")
  }
}

## need the other ones too
std_mu_0 = mean(std$std_mu_0)
std_mu_1 = mean(std$std_mu_1)
std_mu_2 = mean(std$std_mu_2)
std_mu_3 = mean(std$std_mu_3)
std_mu_4 = mean(std$std_mu_4)
std_mu_5 = mean(std$std_mu_5)
std_mu_7 = mean(std$std_mu_7)
std_mu_8 = mean(std$std_mu_8)
std_mu_10 = mean(std$std_mu_11)
std_mu_16 = mean(std$std_mu_16)
std_mu_17 = mean(std$std_mu_17)
std_mu_19 = mean(std$std_mu_19)
std_mu_29 = mean(std$std_mu_29)
std_mu_97 = mean(std$std_mu_97)
std_mu_98 = mean(std$std_mu_98)
std_mu_99 = mean(std$std_mu_99)