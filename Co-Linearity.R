library(dplyr)

test = lm(SEX ~ AGE, data=variables)
test2 = lm(SEX ~ DRIVER, data=variables)
test3 = lm(AGE ~ DRIVER, data=variables)

cat("Colinearity (SEX ~ AGE)", test$coefficients[2], "\n")
cat("Colinearity (SEX ~ DRIVER)", test2$coefficients[2], "\n")
cat("Colinearity (AGE ~ DRIVER)", test3$coefficients[2], "\n")
