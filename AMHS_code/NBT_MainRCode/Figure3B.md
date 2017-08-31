## Figure 3B
```markdown
night <- unlist(tapply(factor(temp$night_symptoms, levels = c("True", "False")), factor(temp$healthCode), 
    function(x) table(x)[[1]]/sum(table(x))))
    
sum(tapply(night, factor(GINA.cal(names(night))), length))

kruskal.test(night, factor(GINA.cal(names(night))))

boxplot(as.numeric(night) ~ factor(GINA.cal(names(night)), levels = c("Uncontrolled", "Partly Controlled", 
    "Well Controlled")))
```
