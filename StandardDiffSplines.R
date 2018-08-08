reg <- predict.glm(glm(SEATBELT ~ (DRIVER*ns(AGE, knots=4) + SEX + ns(AGE, knots=4)), family = "binomial", data = variables),type="response")

propensity <- variables %>% mutate(P_REST = reg)

percents <- propensity %>%
  group_by(SEATBELT) %>%
  mutate(PERCENT = n() / nrow(variables)) %>%
  ungroup(SEATBELT) %>%
  mutate(P_SCORE = PERCENT/P_REST)

## Function Definitions
# f_mu_weighted(ps2 %>% select(AGE), ps2 %>% select(PROPSCORE))
f_mu_weighted <- function(value, weights){
  return(sum(value * weights)/sum(weights))
}

# f_sig_weighted(ps2 %>% select(AGE), ps2 %>% select(PROPSCORE))
f_sig_weighted <- function(value, weights){
  value.mu.weighted <- f_mu_weighted(value, weights)
  sum.weights.squared <- sum(weights)**2
  sum.squared.weights <- sum(weights**2)
  sum.weights <- sum(weights)
  return(sum.weights/(sum.weights.squared - sum.squared.weights) * sum( weights * (value - value.mu.weighted)**2))
}

# f_d_cont_weighted(ps2 %<% filter(SEATBELT == 1), ps2 %<% filter(SEATBELT == 0))
f_d_cont_unweighted <- function(treatment, control, name){
  treatment.mu <- apply(treatment %>% select(names(treatment)[names(treatment) == name]),2,mean)
  treatment.sig <- apply(treatment %>% select(names(treatment)[names(treatment) == name]),2,var)
  control.mu <- apply(control %>% select(names(control)[names(control) == name]),2,mean)
  control.sig <- apply(control %>% select(names(control)[names(control) == name]),2,var)
  return (abs(unname(100 * (treatment.mu - control.mu)/sqrt((treatment.sig + control.sig)/2))))
}

f_d_dich <- function(treatment, control){
  return(100 * (treatment - control) / sqrt(treatment*(1-treatment) - control*(1-control)))
}

# f_d_cont_weighted(ps2 %<% filter(SEATBELT == 1), ps2 %<% filter(SEATBELT == 0))
f_d_cont_weighted <- function(treatment, control, name, weight){
  treatment.mu <- f_mu_weighted(treatment %>% select(names(treatment)[names(treatment) == name]), treatment %>% select(names(treatment)[names(treatment) == weight]))
  treatment.sig <- f_sig_weighted(treatment %>% select(names(treatment)[names(treatment) == name]), treatment %>% select(names(treatment)[names(treatment) == weight]))
  control.mu <- f_mu_weighted(control %>% select(names(control)[names(control) == name]), control %>% select(names(control)[names(control) == weight]))
  control.sig <- f_sig_weighted(control %>% select(names(control)[names(control) == name]), control %>% select(names(control)[names(control) == weight]))
  return (abs(unname(100 * (treatment.mu - control.mu)/sqrt((treatment.sig + control.sig)/2))))
}

### Calculate Propensity Scores
Pr_SEATBELT = length(which(variables$SEATBELT == 1))/length(variables$SEATBELT)

## Standard diff
propensity <- variables %>% mutate(P_SCORE = if_else(SEATBELT == 1, reg, 1-reg)) %>%
  mutate(P_STAB = if_else(SEATBELT == 1, Pr_SEATBELT/P_SCORE, (1-Pr_SEATBELT)/P_SCORE))

treatment <- propensity %>% filter(SEATBELT == 1) 
control <- propensity %>% filter(SEATBELT == 0)

d.weighted.age <- f_d_cont_weighted(treatment, control, "AGE", "P_STAB")
d.unweighted.age <- f_d_cont_unweighted(treatment, control, "AGE")

d.weighted.sex <- f_d_cont_weighted(treatment, control, "SEX", "P_STAB")
d.unweighted.sex <- f_d_cont_unweighted(treatment, control, "SEX")

d.weighted.driver <- f_d_cont_weighted(treatment, control, "DRIVER", "P_STAB")
d.unweighted.driver <- f_d_cont_unweighted(treatment, control, "DRIVER")

std_diff <- data_frame(WEIGHTED = c(d.weighted.age, d.weighted.sex, d.weighted.driver), UNWEIGHTED = c(d.unweighted.age, d.unweighted.sex, d.unweighted.driver))

## PLT Standard Diff
library(ggplot2)

labels <- data_frame(c("AGE", "SEX", "FRONTSEAT"))
plot1<- std_diff %>% ggplot() + geom_point(aes(y=labels, x=std_diff$WEIGHTED)) + scale_y_discrete() + ylab("") + xlab("Weighted")
plot2 <- std_diff %>% ggplot() + geom_point(aes(y=labels, x=std_diff$UNWEIGHTED)) + scale_y_discrete() + ylab("") + xlab("Unweighted") + ggtitle("Standard Diff")
plot1 <- plot1 + scale_x_continuous(limits=c(min(std_diff$WEIGHTED, std_diff$UNWEIGHTED), max(std_diff$WEIGHTED, std_diff$UNWEIGHTED)))
plot2 <- plot2 + scale_x_continuous(limits=c(min(std_diff$WEIGHTED, std_diff$UNWEIGHTED), max(std_diff$WEIGHTED, std_diff$UNWEIGHTED)))

library(grid)

grid.newpage()
grid.draw(rbind(ggplotGrob(plot2), ggplotGrob(plot1), size = "last"))



