## Figure 1A
```markdown
get_scatterplot2 <- function(x, y, my.xlab = "", my.ylab = "") {
    int <- intersect(names(x[!is.na(x)]), names(y[!is.na(y)]))
    xplot <- x[match(int, names(x))]
    yplot <- y[match(int, names(y))]
    forplot = data.frame(xplot, yplot, int)
    myrange = range(c(xplot, yplot))
    myrange <- range(myrange + 0.01)
    myrange <- range(myrange - 0.01)
    fit <- lm(log10(yplot) ~ log10(xplot))
    corr <- cor(log10(yplot), log10(xplot))
    out <- cor.test(log10(yplot), log10(xplot))
    mylabel = bquote(rho == .(format(corr, digits = 2)))
    
    textplot(log10(forplot$xplot), log10(forplot$yplot), row.names(forplot), xlab = my.xlab, 
        ylab = my.ylab, show.lines = FALSE, cex = 0.7)
    abline(fit, lty = 2)
    legend("bottomright", legend = as.expression(mylabel), bty = "n")
    out
}

mytable <- table(all.state)
sum(mytable)
mytable <- mytable[mytable > 5]
app.state.prev <- mytable/sum(mytable)
cdc.perc.prev = as.numeric(cdc.prev$cdc.prev)
names(cdc.perc.prev) = cdc.prev$state
get_scatterplot2(app.state.prev * 100, cdc.perc.prev * 100, my.xlab = "Log10 Percentage Prevalence (AHA)", 
    my.ylab = "Log10 Percentage Prevalence (CDC)")
```
