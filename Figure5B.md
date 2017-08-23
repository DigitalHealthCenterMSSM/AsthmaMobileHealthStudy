## Figure 5B
```markdown
GINA.m = matrix(NA, nrow = nrow(mile.data), ncol = 4)
colnames(GINA.m) = c("DaySym", "NighSym", "Reliever", "Activity")
for (i in 1:nrow(mile.data)) {
    GINA.m[i, 1] = is.element(mile.data[i, "symptoms2"], c("[1]", "[2]", "[3]")) + 0  ### day sym more than twice a week
    GINA.m[i, 2] = !is.element(mile.data[i, "nights2"], c("", "[5]", "[6]")) + 0  ### any night sym
    GINA.m[i, 3] = is.element(mile.data[i, "past_month_quick_relief2"], c("[1]", "[2]", "[3]")) + 
        0  ### more than twice a week
    GINA.m[i, 4] = !is.element(mile.data[i, "limited.activity2"], c("", "[5]", "[6]")) + 
        0  ### any limited activity
}
GINA.v = apply(GINA.m, 1, sum)
GINA.indi = GINA.v
GINA.indi[GINA.v > 2] = "Uncontrolled"
GINA.indi[GINA.v > 0 & GINA.v < 3] = "Partly Controlled"
GINA.indi[GINA.v == 0] = "Well Controlled"
table(GINA.indi)
Mile.GINA = data.frame(healthCode = mile.data[, "healthCode"], GINA.indi)
get_cont.table = function(user.temp) {
    table(GINA.cal(user.temp), Mile.GINA[match(user.temp, Mile.GINA$healthCode), ]$GINA.indi)
}
get_cont.table(cohorts$milestone)
get_cont.table(cohorts$robust)
get_cont.table(cohorts$baseline)
```
