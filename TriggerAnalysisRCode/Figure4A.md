## Figure 4A
```markdown
barplot(t(forplot), col = trig.colors)
legend("topleft", trig.lab1, fill = trig.colors)
################################################################ 
date <- tapply(temp$date, factor(temp$clust.ext), as.Date)
temp <- temp.clust2
mycut <- mycut.5day
trig <- tapply(temp$get_worse, factor(temp$clust.ext), as.character)
date <- tapply(temp$date, factor(temp$clust.ext), as.Date)
trig.out <- list()
trig.nout <- list()
for (i in 1:length(date)) {
    trig.out[[i]] <- get_dist(trig[[i]], date[[i]], mycut, 10)[[1]]
    trig.nout[[i]] <- get_dist(trig[[i]], date[[i]], mycut, 10)[[2]]
}
heat.out <- t(do.call(rbind, lapply(trig.out, function(x) x[, trig.lev1 %in% "12"])))
colnames(heat.out) <- names(trig)
get_tmax.plot <- function(astation, mycol, data.levels) {
    data <- lapply(mytemp, function(x) as.numeric(x[match(date.levels, names(x))]))
    data <- do.call(rbind, data)
    data <- data[row.names(data) %in% astation, ]/10
    m <- apply(data, 2, mean, na.rm = T)
    s <- apply(data, 2, sd, na.rm = T)
    e <- get_error(apply(data, 2, sum, na.rm = T), s)
    # plot.mat=rbind(m+e,m,m-e)
    # main.lab<-as.character(apploc[match(azip,apploc$zip.zcta),]$zip.place)
    # plot(smooth.spline(m),ylim=c(0,50),main='',xlab='',ylab='mean maximum temp
    # (celsius)',col=mycol,type='l',lty=2)
    plot.mat = my.bbands(m, n = 4, sd = 1)
    matplot(plot.mat[, 1:3], ylim = c(0, 50), main = "", xlab = "", ylab = "mean maximum temp (celsius)", 
        col = mycol, type = "l", lwd = c(1, 2, 1), lty = c(1, 2, 1), xaxt = "n")
    # arrows(1:length(m),as.numeric(m-e),1:length(m),as.numeric(m+e),code=3,length=0.05,angle=90)
    out <- 1:nrow(plot.mat)
}
```
