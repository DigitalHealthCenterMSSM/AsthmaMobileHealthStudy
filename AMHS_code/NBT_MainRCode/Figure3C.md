## Figure 3C
```markdown
qr.use <- unlist(tapply(factor(temp$use_qr, levels = c("True", "False")), factor(temp$healthCode), 
    function(x) table(x)[[1]]/sum(table(x))))
    
table(is.na(qr.use))

sum(tapply(qr.use, factor(GINA.cal(names(qr.use))), length))

kruskal.test(qr.use, factor(GINA.cal(names(qr.use))))

boxplot(as.numeric(qr.use) ~ factor(GINA.cal(names(qr.use)), levels = c("Uncontrolled", "Partly Controlled", 
    "Well Controlled")))
```
