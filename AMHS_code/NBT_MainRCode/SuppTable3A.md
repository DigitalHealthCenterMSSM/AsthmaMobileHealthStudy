## Supplementary Table 3A
```markdown
edate.data <- data.frame(healthCode = cohorts$baseline, edate = as.Date(enrol.date, format = "%Y-%m-%d"))
mymonth.cut <- as.Date(c("2015-03-09", "2015-04-08", "2015-05-09", "2015-06-09", "2015-07-10", 
    "2015-08-10", "2015-09-10"), format = "%Y-%m-%d")
ecount.daily = table(cut(as.Date(Dai.data$date), mymonth.cut))
ecount.weekly = table(cut(as.Date(week.data$date), mymonth.cut))
get_ecount = function(x) {
    table((cut(as.Date(edate.data[edate.data$healthCode %in% x, ]$edate), mymonth.cut)))
}
ecount.cohort = do.call(cbind, lapply(cohorts, get_ecount))
########################## 
enrol.m = enrollment[, 2:5]
enrol.m = as.matrix(enrol.m)
enrol.m = matrix(as.numeric(as.vector(enrol.m)), nrow = nrow(enrol.m))
enrol.s = matrix(NA, nrow = 6, ncol = 2)
colnames(enrol.s) = c("Downloads", "Enrolled")
month.cut = 1:6
month.cut[1] = 0
month.cut[2] = my.convert.day("2015-04-08")
month.cut[3] = my.convert.day("2015-05-09")
month.cut[4] = my.convert.day("2015-06-09")
month.cut[5] = my.convert.day("2015-07-10")
month.cut[6] = my.convert.day("2015-08-10")
month.cut[7] = my.convert.day("2015-09-10")
for (i in 1:6) {
    pick = enrol.m[, 1] == i
    enrol.s[i, 1] = sum(enrol.m[pick, 4], na.rm = T)
    temp = max(enrol.m[pick, 3], na.rm = T)
    temp1 = 0
    if (i > 1) {
        temp1 = max(enrol.m[enrol.m[, 1] == (i - 1), 3], na.rm = T)
    }
    enrol.s[i, 2] = temp - temp1
    
}
enrol.s1 = cbind(downloads = enrol.s[, 1], daily = ecount.daily, daily.dist = round(ecount.daily/sum(ecount.daily), 
    2), weekly = ecount.weekly, weekly.dist = round(ecount.weekly/sum(ecount.weekly), 2), 
    enrolled = enrol.s[, 2], enrol.perc = round(enrol.s[, 2]/enrol.s[, 1], 2), baseline = ecount.cohort[, 
        1], baseline.perc = round(ecount.cohort[, 1]/enrol.s[, 1], 2), robust = ecount.cohort[, 
        2], robust.perc = round(ecount.cohort[, 2]/enrol.s[, 1], 2), milestone = ecount.cohort[, 
        3], milestone.perc = round(ecount.cohort[, 3]/enrol.s[, 1], 2))

```
