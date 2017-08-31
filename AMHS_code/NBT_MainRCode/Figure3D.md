## Figure 3D
```markdown
medi <- read.table("Daily_medicine_ans_by_date_last_response.tsv", sep = "\t", header = T)
medi <- medi[medi$healthCode %in% cohorts$robust, ]
medi$medicine <- factor(medi$medicine, levels <- c(1, 2, 3))
Dai.data$medicine = medi[match(Dai.data$healthCode, medi$healthCode), ]$medicine
comp2 <- unlist(tapply(factor(medi$medicine), factor(medi$healthCode), function(x) (table(x)[[1]] + 
    table(x)[[2]])/sum(table(x))))
    
sum(tapply(comp2, factor(GINA.cal(names(comp2))), length))

kruskal.test(comp2, factor(GINA.cal(names(comp2))))

boxplot(as.numeric(comp2) ~ factor(GINA.cal(names(comp2)), levels = c("Uncontrolled", "Partly Controlled", 
    "Well Controlled")))
```
