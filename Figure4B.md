## Figure 4B
```markdown
par(mar = c(6.1, 6.1, 8.1, 8.1))
temp <- temp.clust2
extrap.zcta <- temp.zip$clust.ext
region.zip <- tapply(temp.clust2$zip.zcta, temp.clust2$clust.ext, function(x) levels(factor(x)))
mymonth <- cut(as.Date(temp.clust2$date), "month", labels = c("Mar", "Apr", "May", "Jun", 
    "Jul", "Aug", "Sep"))
mypollen <- mapply(function(x, y) pollen.data[match(x, pollen.data$ZIP), colnames(pollen.data) %in% 
    y], extrap.zcta, mymonth)
a <- mymonth[extrap.zcta %in% pollen.data$ZIP & extrap.zcta %in% region.zip[[1]]]
b <- mypollen[extrap.zcta %in% pollen.data$ZIP & extrap.zcta %in% region.zip[[1]]]
pollen.north <- tapply(b, a, mean, na.rm = T)
a <- mymonth[extrap.zcta %in% pollen.data$ZIP & extrap.zcta %in% region.zip[[2]]]
b <- mypollen[extrap.zcta %in% pollen.data$ZIP & extrap.zcta %in% region.zip[[2]]]
pollen.south <- tapply(b, a, mean, na.rm = T)
clust.col <- c("blue", "red", "orange", "green", "purple")
check <- pollen.out[, apply(pollen.out, 2, function(x) sum(!is.na(x))) >= 5][, 1:2]
n.check <- nout[apply(pollen.out, 2, function(x) sum(!is.na(x))) >= 5][1:2]
mycut = date.levels
trig.out <- list()
trig.nout <- list()
for (i in 1:length(date)) {
    trig.out[[i]] <- get_dist(trig[[i]], date[[i]], mycut, 10)[[1]]
    trig.nout[[i]] <- get_dist(trig[[i]], date[[i]], mycut, 10)[[2]]
}
pollen.out <- t(do.call(rbind, lapply(trig.out, function(x) x[, trig.lev1 %in% "11"])))
colnames(pollen.out) <- names(trig)
main.lab <- levels(temp$clust.ext)
for (i in 1:ncol(check)) {
    azip <- na.omit(as.character(unlist(tapply(temp$zip.zcta, temp$clust.ext, unique)[colnames(check)[i]])))
    nzip <- as.numeric(unlist(n.check[[i]]))
    myplot <- get_meanplot(azip, "", clust.col[i])
    par(new = TRUE)
    out
}
for (i in 1:ncol(check)) {
    azip <- na.omit(as.character(unlist(tapply(temp$zip.zcta, factor(temp$clust.ext, colnames(check)), 
        unique)[colnames(check)[i]])))
    nzip <- as.numeric(unlist(n.check[[i]]))
    par(new = T)
    y = check[, i]
    y <- y[!is.na(y)]
    forplot = BBands(y, n = 10, sd = 1)
    matplot(match(names(y), date.levels), forplot[, 1:3], type = "l", ylim = c(0, 0.4), xlim = c(1, 
        185), axes = F, ylab = "", xlab = "", col = reg.colors[i], xaxt = "n", lty = c(2, 
        1, 2), lwd = c(1, 2, 1))
}
legend("topright", col = c("red", "blue", "red", "blue"), c("Pollen count (south)", "Pollen count (north)", 
    "% Trigger Dist (south)", "% Trigger Dist (north)"), lty = c(2, 2, 1, 1), lwd = c(1, 
    1, 2, 2), cex = 0.75)
axis(4, at = 0:10/10, lab = 0:10/(2 * 5), las = 2)
axis(1, at = na.omit(match(mycut.month, date.levels)), na.omit(date.levels[match(mycut.month, 
    date.levels)]), las = 2)
mtext("% Trigger Dist.", side = 4, line = 3)
```
