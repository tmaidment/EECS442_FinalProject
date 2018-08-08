## SCRIPT SETUP
library(dplyr)

setwd('Documents/EECS442/causal-final-project')
#dat = read.csv("./FARS2015NationalCSV/person.csv", header = TRUE)
dat = read.csv("./GES2015csv/PERSON.csv", header = TRUE)
#head(dat)

## IMPLEMENTATION

# select the variables we're using
test <- select(dat, INJ_SEV, REST_USE, REST_MIS, PER_TYP, AGE, SEX, AIR_BAG)

## Linear Regression
linout <- filter(test, INJ_SEV == 1 | INJ_SEV == 2 | INJ_SEV == 3 | INJ_SEV == 4)

lin.reg <- lm(REST_USE ~ AIR_BAG, data=linout)

## logistic regression

logout <- test %>%
  filter(INJ_SEV == 1 | INJ_SEV == 2 | INJ_SEV == 3 | INJ_SEV == 4) %>%
  mutate(DEATH = if_else(INJ_SEV == 1 | INJ_SEV == 2 | INJ_SEV == 3, 0, 1))

logout.reg <- lm(DEATH ~ REST_USE, data=logout)

glm(formula = DEATH ~ REST_USE, family = "binomial", data = logout)

## propensity score
ps <- logout %>%
  filter(REST_USE == 3 | REST_USE == 0) %>%
  mutate(SEATBELT = if_else(REST_USE == 3, 1, 0))

#ps.score <- predict.glm(glm(SEATBELT ~ SEX, family = "binomial", data = ps), type="response")
ps.score.death.seatbelt <- predict.glm(glm(DEATH~SEATBELT, family = "binomial", data = ps), type="response")

ps2 <- ps %>%
  filter(SEX == 1 | SEX == 2, AGE < 200)

ps2.score <- predict.glm(glm(SEATBELT ~ SEX, family = "binomial", data = ps2), type="response")
  
  ps2$PER_TYP = factor(ps2$PER_TYP)
  
ps2.score.per_type <- predict.glm(glm(SEATBELT ~ PER_TYP, family = "binomial", data = ps2), type="response")

## logist w death and seatbelt
glm(formula = DEATH ~ SEATBELT, family = "binomial", data = ps2)

## has no effects           
glm(formula = DEATH ~ AGE, family = "binomial", data = ps2)

glm(formula = DEATH ~ SEX, family = "binomial", data = ps2)

## to calc a fraction
length(which(ps2$SEATBELT == 1))/length(ps2$SEATBELT)

d.dich <- function(treatment, control){
  return(100 * (treatment - control) / sqrt(treatment*(1-treatment) - control*(1-control)))
}
  
d.cont <- function(treatment, control){
  return(100 * (mean(treatment) - mean(control))/sqrt((var(treatment) + var(control))/2))
}

## example input - unweighted
# d.cont2(ps2 %>% group_by(SEATBELT) %>% summarize(Mean = mean(AGE), Var = var(AGE)))
d.cont2 <- function(smmry){
  return(100 * (smmry$Mean[1] - smmry$Mean[2])/sqrt((smmry$Var[1] + smmry$Var[2])/2))
}

## example input - weighted
Pr_A = length(which(ps2$SEATBELT == 1))/length(ps2$SEATBELT)
ps2.score.age <- predict.glm(glm(SEATBELT ~ AGE, family = "binomial", data = ps2), type="response")
ps2 <- ps2 %>% mutate(PROPSCORE = if_else(SEATBELT == 1, ps2.score.age, 1-ps2.score.age))
ps2 <- ps2 %>% mutate(STAB_WEIGHT = if_else(SEATBELT == 1, Pr_A/PROPSCORE, (1-Pr_A)/PROPSCORE))
treatment <- ps2 %>% filter(SEATBELT == 1) 
control <- ps2 %>% filter(SEATBELT == 0)
# d.cont3(mu.weighted(treatment %>% select(AGE), treatment %>% select(PROPSCORE)), )
d.cont3 <- function(treatment, control){
  treatment.mu <- mu.weighted(treatment %>% select(AGE), treatment %>% select(STAB_WEIGHT))
  treatment.sig <- sig.weighted(treatment %>% select(AGE), treatment %>% select(STAB_WEIGHT))
  control.mu <- mu.weighted(control %>% select(AGE), control %>% select(STAB_WEIGHT))
  control.sig <- sig.weighted(control %>% select(AGE), control %>% select(STAB_WEIGHT))
  return(100 * (treatment.mu - control.mu)/sqrt((treatment.sig + control.sig)/2))
}

# mu.weighted(ps2 %>% select(AGE), ps2 %>% select(PROPSCORE))
mu.weighted <- function(value, weights){
  return(sum(value * weights)/sum(weights))
}

# sig.weighted(ps2 %>% select(AGE), ps2 %>% select(PROPSCORE))
sig.weighted <- function(value, weights){
  value.mu.weighted <- mu.weighted(value, weights)
  sum.weights.squared <- sum(weights)**2
  sum.squared.weights <- sum(weights**2)
  sum.weights <- sum(weights)
  return(sum.weights/(sum.weights.squared - sum.squared.weights) * sum( weights * (value - value.mu.weighted)**2))
}

## VIF
vif <- function(r.squared){
  return(1/1-r.squared)
}

