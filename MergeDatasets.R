library(dplyr)
library(splines)

## VIF function definition
vif <- function(r.squared){
  return(1/1-r.squared)
}

dat = read.csv("./GES2015csv/PERSON.csv", header = TRUE)

# Clean Up Data
variables1 <- dat %>%
  select(INJ_SEV, REST_USE, REST_MIS, SEAT_POS, AGE, SEX, AIR_BAG)

dat2 = read.csv("./FARS2015NationalCSV/person.csv")

variables2 <- dat2 %>%
  select(INJ_SEV, REST_USE, REST_MIS, SEAT_POS, AGE, SEX, AIR_BAG)

variables <- variables1 %>% bind_rows(variables2) %>%
  filter(INJ_SEV <= 4, AGE < 200, SEX == 2 | SEX == 1, SEAT_POS > 10 & SEAT_POS < 50, REST_MIS == 0, REST_USE != 96) %>%
  mutate(DEATH = if_else(INJ_SEV <= 3, 0, 1), SEATBELT = if_else(REST_USE == 3, 1, 0), DRIVER = if_else((SEAT_POS < 20), 1, 0))
variables$REST_USE = factor(variables$REST_USE)