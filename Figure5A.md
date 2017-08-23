## Figure 5A
```markdown
WE.day.index = unlist(lapply(week.data[, "date"], my.convert.day))
user.set <- cohorts$robust
Last.week = matrix(NA, nrow = length(user.set), ncol = ncol(week.data) + 1)
First.week = matrix(NA, nrow = length(user.set), ncol = ncol(week.data) + 1)
for (i in 1:length(user.set)) {
    cur.user = user.set[i]
    pick = which(week.data[, "healthCode"] == cur.user)
    if (length(pick) > 0) {
        if (length(pick) == 1) {
            Last.week[i, ] = c(WE.day.index[pick], as.matrix(week.data)[pick, ])
            First.week[i, ] = Last.week[i, ]
        } else {
            temp = week.data[pick, ]
            temp1 = WE.day.index[pick]
            Last.week[i, ] = c(max(temp1), as.matrix(temp)[which.max(temp1)[1], ])
            First.week[i, ] = c(min(temp1), as.matrix(temp)[which.min(temp1)[1], ])
        }
    }
}
colnames(Last.week) = c("Day", colnames(week.data))
colnames(First.week) = c("Day", colnames(week.data))
Last.w.day = as.numeric(Last.week[, 1])
First.w.day = as.numeric(First.week[, 1])
Len.w = Last.w.day - First.w.day
Len.w[is.na(Len.w)] = 0
hist(Len.w[Len.w > 0], xlab = "# of days betwen the first and last weekly survey", main = "")
a = 90
Last.week.3m = Last.week[Len.w > a, ]
First.week.3m = First.week[Len.w > a, ]
user.3m = user.set[Len.w > a]
col.n = c("asthma_doc_visit", "emergency_room", "prednisone", "limitations", "missed_work", 
    "admission")
p.list = rep(NA, length(col.n))
for (k in 1:length(col.n)) {
    cur.n = col.n[k]
    for (i in 1:nrow(Last.week)) {
        cur = Last.week[i, cur.n]
        if (!is.na(cur)) {
            Last.week[i, cur.n] = gsub("\\ ", "", cur)
        }
        cur = First.week[i, cur.n]
        if (!is.na(cur)) {
            First.week[i, cur.n] = gsub("\\ ", "", cur)
        }
    }
    x.v = Last.week[, cur.n] == "True"
    x.v = x.v + 0
    y.v = First.week[, cur.n] == "True"
    y.v = y.v + 0
    temp = wilcox.test(x.v, y.v, paired = TRUE)
    p.list[k] = temp$p.value
}
round(p.list, 3)
length(user.3m)
cur.n = "limitations"
table(Last.week = factor(Last.week.3m[, cur.n], levels = c("True", "False")), First.week = factor(First.week.3m[, 
    cur.n], levels = c("True", "False")))
wilcox.test(as.numeric(factor(Last.week.3m[, cur.n], levels = c("True", "False"))), as.numeric(factor(First.week.3m[, 
    cur.n], levels = c("True", "False"))), paired = T)
table(Last.week = factor(Last.week[, cur.n], levels = c("True", "False")), First.week = factor(First.week[, 
    cur.n], levels = c("True", "False")))
wilcox.test(as.numeric(factor(Last.week[, cur.n], levels = c("True", "False"))), as.numeric(factor(First.week[, 
    cur.n], levels = c("True", "False"))), paired = T)

```
