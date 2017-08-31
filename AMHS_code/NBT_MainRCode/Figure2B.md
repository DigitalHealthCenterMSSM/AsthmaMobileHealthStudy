## Figure 2B

```markdown
clinical.data <- His.data[, c("healthCode", "times_hospitalized", "emergency", "intubated", 
    "symptoms", "nights", "limited activity")]
activity <- factor(clinical.data[, "limited activity"])
out <- get_demo.plot(activity, clinical.data)

ggplot(out[[1]], aes(x = Period, y = temp.Dist, fill = name)) + geom_bar(stat = "identity", 
    position = "stack")
    
chisq.test(out[[2]], simulate.p.value = T)
```
