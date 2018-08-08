library(dplyr)

test = lm(SEX ~ AGE, data=variables)
test2 = lm(SEX ~ DRIVER, data=variables)
test3 = lm(AGE ~ DRIVER, data=variables)

cat("R^2 (SEX ~ AGE)", summary(test)$r.squared, "\n")
cat("R^2 (SEX ~ DRIVER)", summary(test2)$r.squared, "\n")
cat("R^2 (AGE ~ DRIVER)", summary(test3)$r.squared, "\n")

cat("VIF (SEX ~ AGE)", vif(summary(test)$r.squared), "\n")
cat("VIF (SEX ~ DRIVER)", vif(summary(test2)$r.squared), "\n")
cat("VIF (AGE ~ DRIVER)", vif(summary(test3)$r.squared), "\n")
