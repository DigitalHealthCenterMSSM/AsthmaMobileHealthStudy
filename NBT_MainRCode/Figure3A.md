## Figure 3A
```markdown
temp = Dai.data[Dai.data$healthCode %in% cohorts$robust, ]
day <- unlist(tapply(factor(temp$day_symptoms, levels = c("True", "False")), factor(temp$healthCode), 
    function(x) table(x)[[1]]/sum(table(x))))
sum(tapply(day, factor(GINA.cal(names(day))), length))
kruskal.test(day, factor(GINA.cal(names(day))))
boxplot(as.numeric(day) ~ factor(GINA.cal(names(day)), levels = c("Uncontrolled", "Partly Controlled", 
    "Well Controlled")))
```
