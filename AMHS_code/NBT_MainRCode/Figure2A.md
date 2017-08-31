## Figure 2A
```markdown
out <- get_demo.plot(gender, mydata)

ggplot(out[[1]], aes(x = Period, y = temp.Dist, fill = name)) + geom_bar(stat = "identity", 
    position = "stack")
    
chisq.test(out[[2]])

gina = factor(GINA.cal(unlist(robust.enrol)))
mydata = data.frame(healthCode = unlist(robust.enrol), gina)
out <- get_demo.plot(gina, mydata)

ggplot(out[[1]], aes(x = Period, y = temp.Dist, fill = name)) + geom_bar(stat = "identity", 
    position = "stack")

chisq.test(out[[2]])
```
