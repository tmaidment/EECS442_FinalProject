library(dplyr)

## Possibly set WD
setwd('Documents/EECS442/causal-final-project')

# Load in GES 2015 data
dat = read.csv("./GES2015csv/PERSON.csv", header = TRUE)

# Clean Up Data
variables <- dat %>%
  select(INJ_SEV, REST_USE, REST_MIS, PER_TYP, AGE, SEX, AIR_BAG) %>%
  filter(INJ_SEV == 1 | INJ_SEV == 2 | INJ_SEV == 3 | INJ_SEV == 4, REST_USE == 3 | REST_USE == 0, AGE < 200, SEX == 2 | SEX == 1) %>%
  mutate(DEATH = if_else(INJ_SEV == 1 | INJ_SEV == 2 | INJ_SEV == 3, 0, 1), SEATBELT = if_else(REST_USE == 3, 1, 0))
  
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

## AGE
pscore.age <- predict.glm(glm(SEATBELT ~ AGE, family = "binomial", data = variables), type="response")
propensity.age <- variables %>% mutate(P_AGE = if_else(SEATBELT == 1, pscore.age, 1-pscore.age)) %>%
  mutate(P_AGE_STAB = if_else(SEATBELT == 1, Pr_SEATBELT/P_AGE, (1-Pr_SEATBELT)/P_AGE))

treatment.age <- propensity.age %>% filter(SEATBELT == 1) 
control.age <- propensity.age %>% filter(SEATBELT == 0)

d.weighted.age <- f_d_cont_weighted(treatment.age, control.age, "AGE", "P_AGE_STAB")
d.unweighted.age <- f_d_cont_unweighted(treatment.age, control.age, "AGE")

## SEX
pscore.sex <- predict.glm(glm(SEATBELT ~ SEX, family = "binomial", data = variables), type="response")
propensity.sex <- variables %>% mutate(P_SEX = if_else(SEATBELT == 1, pscore.sex, 1-pscore.sex)) %>%
  mutate(P_SEX_STAB = if_else(SEATBELT == 1, Pr_SEATBELT/P_SEX, (1-Pr_SEATBELT)/P_SEX))

treatment.sex <- propensity.sex %>% filter(SEATBELT == 1) 
control.sex <- propensity.sex %>% filter(SEATBELT == 0)

d.weighted.sex <- f_d_cont_weighted(treatment.sex, control.sex, "SEX", "P_SEX_STAB")
d.unweighted.sex <- f_d_cont_unweighted(treatment.sex, control.sex, "SEX")

## SEX + AGE
pscore.sex.age <- predict.glm(glm(SEATBELT ~ SEX + AGE, family = "binomial", data = variables), type="response")
propensity.sex.age <- variables %>% mutate(P_SEX_AGE = if_else(SEATBELT == 1, pscore.sex.age, 1-pscore.sex.age)) %>%
  mutate(P_SEX_AGE_STAB = if_else(SEATBELT == 1, Pr_SEATBELT/P_SEX_AGE, (1-Pr_SEATBELT)/P_SEX_AGE))

treatment.sex.age <- propensity.sex.age %>% filter(SEATBELT == 1) 
control.sex.age <- propensity.sex.age %>% filter(SEATBELT == 0)

d.weighted.age.sex <- f_d_cont_weighted(treatment.sex.age, control.sex.age, "AGE", "P_SEX_AGE_STAB")
d.unweighted.age.sex <- f_d_cont_unweighted(treatment.sex.age, control.sex.age, "AGE")

d.weighted.sex.age <- f_d_cont_weighted(treatment.sex.age, control.sex.age, "SEX", "P_SEX_AGE_STAB")
d.unweighted.sex.age <- f_d_cont_unweighted(treatment.sex.age, control.sex.age, "SEX")


