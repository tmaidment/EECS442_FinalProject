library(dplyr)

## Normal Logistic Regression
reg <- predict.glm(glm(SEATBELT ~ (DRIVER*AGE**2 + SEX + AGE**2), family = "binomial", data = variables),type="response")

propensity <- variables %>% mutate(P_REST = reg)

percents <- propensity %>%
  group_by(SEATBELT) %>%
  mutate(PERCENT = n() / nrow(variables)) %>%
  ungroup(SEATBELT) %>%
  mutate(P_SCORE = PERCENT/P_REST)


death <- glm(DEATH ~ SEATBELT, weights=P_SCORE, family="binomial", data=percents)

cat("mean", mean(percents$P_SCORE), "max", max(percents$P_SCORE), "min", min(percents$P_SCORE), "\n")
exp(coef(death))
summary(death)$coefficients[, 2]**2
