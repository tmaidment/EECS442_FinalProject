
## Normal Logistic Regression

reg <- nnet::multinom(REST_USE ~ DRIVER*ns(AGE, knots=4) + SEX + ns(AGE, knots=4), data = variables)

estimate <- data.frame(fitted(reg))

propensity <- variables %>% mutate(P_REST = 0)

# get the weights into the table.
for (i in 1:nrow(variables)){
  propensity[i, match("P_REST", colnames(propensity))] = select(estimate, num_range("X", propensity[i, match("REST_USE", colnames(propensity))]))[i,]
}

percents <- propensity %>%
  group_by(REST_USE) %>%
  mutate(PERCENT = n() / nrow(variables)) %>%
  ungroup(REST_USE) %>%
  mutate(P_SCORE = PERCENT/P_REST)

death <- glm(DEATH ~ REST_USE, weights=P_SCORE, family="binomial", data=percents)

cat("mean", mean(percents$P_SCORE), "max", max(percents$P_SCORE), "min", min(percents$P_SCORE), "\n")
exp(coef(death))
