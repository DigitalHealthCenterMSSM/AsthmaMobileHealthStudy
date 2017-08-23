## Figure 2C
```markdown
symptoms <- factor(clinical.data$symptoms)
out <- get_demo.plot(symptoms, clinical.data)
ggplot(out[[1]], aes(x = Period, y = temp.Dist, fill = name)) + geom_bar(stat = "identity", 
    position = "stack")
chisq.test(out[[2]], simulate.p.value = T)
```
