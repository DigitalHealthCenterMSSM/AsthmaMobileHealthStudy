## Figures 2 D,E + Supplementary Table 3
```markdown
rob.date = edate.data[match(cohorts$robust, edate.data$healthCode), ]$edate
seq.day = seq(from = as.Date(range(rob.date)[1]), to = as.Date(range(rob.date)[2]), by = "day")
rob.ind = match(rob.date, seq.day)
rob.gina = GINA.cal(cohorts$robust)
cf1 = rob.gina
y = ifelse(cf1 == "Uncontrolled", 1, 0)
x = rob.ind
fit = glm(factor(y) ~ x, family = binomial())
summary(fit)
confint(fit, parm = "x")
exp(coef(fit)["x"])
exp(confint(fit, parm = "x"))
cdplot(factor(y) ~ x, ylab = "Well Controlled", xlab = "Enrollment Date")
rob.date = edate.data[match(cohorts$robust, edate.data$healthCode), ]$edate
seq.day = seq(from = as.Date(range(rob.date)[1]), to = as.Date(range(rob.date)[2]), by = "day")
rob.ind = match(rob.date, seq.day)
rob.gina = GINA.cal(cohorts$robust)
cf2 = gender[match(cohorts$robust, healthCode)]
x = rob.ind[!is.na(cf2)]
y = ifelse(cf2[!is.na(cf2)] == "Female", 1, 0)
fit = glm(factor(y) ~ x, family = binomial())
summary(fit)
confint(fit, parm = "x")
exp(coef(fit)["x"])
exp(confint(fit, parm = "x"))
cdplot(factor(y) ~ x, ylab = "Gender", xlab = "Enrollment Date")
### use negative version opposite coding
cf3 = clinical.data[match(cohorts$robust, clinical.data$healthCode), "limited activity"]
cf3 = as.numeric(factor(cf3))
y = cf3[!is.na(cf3)]
x = rob.ind[!is.na(cf3)]
fit = lm(y ~ x)
summary(fit)
length(y)
coef(fit)["x"]
confint(fit, parm = "x")
####### 
cf4 = clinical.data[match(cohorts$robust, clinical.data$healthCode), "symptoms"]
cf4 = as.numeric(factor(cf4))
y = cf4[!is.na(cf4)]
x = rob.ind[!is.na(cf4)]
fit = lm(y ~ x)
length(y)
summary(fit)
coef(fit)["x"]
confint(fit, parm = "x")
```
