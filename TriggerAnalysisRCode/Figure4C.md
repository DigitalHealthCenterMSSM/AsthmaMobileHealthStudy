## Figure 4C
```markdown
par(mar = c(6.1, 6.1, 8.1, 8.1))
check <- heat.out[, apply(heat.out, 2, function(x) sum(!is.na(x))) >= 5]
n.check <- nout[apply(heat.out, 2, function(x) sum(!is.na(x))) >= 5]
myplot <- mycut.5day
mytemp <- my.tmax
for (i in 1:ncol(check)) {
    astation <- na.omit(as.character(unlist(tapply(temp$station, factor(temp$clust.ext, colnames(check)), 
        unique)[colnames(check)[i]])))
    azip <- na.omit(as.character(unlist(tapply(temp$zip.zcta, factor(temp$clust.ext, colnames(check)), 
        unique)[colnames(check)[i]])))
    myplot <- get_tmax.plot(astation, clust.col[i], date.levels)
    nzip <- as.numeric(unlist(n.check[[i]]))
    par(new = TRUE)
}
reg.colors = c("blue", "red")
for (i in 1:ncol(check)) {
    azip <- na.omit(as.character(unlist(tapply(temp$zip.zcta, factor(temp$clust.ext, colnames(check)), 
        unique)[colnames(check)[i]])))
    nzip <- as.numeric(unlist(n.check[[i]]))
    par(new = T)
    y = check[, i]
    forplot <- y[!is.na(y)]
    matplot(match(names(forplot), date.levels), my.bbands(forplot, 10, 1), type = "l", ylim = c(0, 
        0.1), xlim = c(1, 185), axes = F, ylab = "", xlab = "", col = reg.colors[i], lty = c(2, 
        1, 2), lwd = c(1, 2, 1))
}
legend("topleft", col = c("red", "blue", "red", "blue"), c("Max temp. (south)", "Max temp. (north)", 
    "% Trigger Dist (south)", "% Trigger Dist (north)"), lty = c(2, 2, 1, 1), lwd = c(1, 
    1, 2, 2), cex = 0.75)
axis(4, at = 0:10/100, lab = 0:10/(100), las = 2)
axis(1, at = na.omit(match(mycut.month, date.levels)), na.omit(date.levels[match(mycut.month, 
    date.levels)]), las = 2)
mtext("% Trigger Dist.", side = 4, line = 3)
################################################################ 
pollen.data$ZIP <- as.character(pollen.data$ZIP)
pollen.data[1, 1] <- "07677"
get_meanplot <- function(azip, main.lab, mycol) {
    data <- as.matrix(pollen.data[pollen.data$ZIP %in% azip, 2:13])
    pollen.zip <- pollen.data[pollen.data$ZIP %in% azip, ]$ZIP
    m <- apply(data, 2, mean)
    s <- apply(data, 2, sd)
    e <- get_error(apply(data, 2, sum), s)
    # main.lab<-as.character(apploc[match(azip,apploc$zip.zcta),]$zip.place)
    # main.lab=main.lab
    out <- plot(1:12 + 0.5, m, ylim = c(0, 10), xlim = c(3, 9), main = main.lab, ylab = "mean pollen count", 
        type = "l", col = mycol, lty = 2, xaxt = "n", las = 2, xlab = "")
    arrows(1:12 + 0.5, as.numeric(m - e), 1:12 + 0.5, as.numeric(m + e), code = 3, xlim = c(3, 
        9), length = 0.05, angle = 90, col = mycol)
    pollen.zip
}
################################################################ 
temp <- temp.zip
wild <- data.frame(wild.data)
wa.fires = as.character(wild[wild$State %in% "Washington", ]$Name)
wa.data <- WA[, 6:13]
mymat <- apply(wa.data[, 2:ncol(wa.data)], 2, function(x) tapply(x, wa.data$pm2pt5.date, 
    mean, na.rm = T))
mymat <- mymat[-1, ]
dat.mat = BBands(mymat[, 6][!is.na(mymat[, 6])], n = 2, "SMA", sd = 1)[, 1:3]
matplot(match(names(mymat[, 6][!is.na(mymat[, 6])]), date.levels), dat.mat, type = "l", ylim = c(0, 
    200), xlim = c(1, 185), xaxt = "n", lty = c(1, 2, 1), xlab = "", ylab = "Max AQI", col = "black")
axis(1, at = na.omit(match(mycut.month, date.levels)), na.omit(date.levels[match(mycut.month, 
    date.levels)]), las = 2)
get_zip.nearby <- function(x, dist) {
    cond <- rdist.earth(loc.data[, c("long", "lat")], t(matrix(c(as.numeric(x$Longitude), 
        as.numeric(x$Latitude))))) <= dist
    unique(as.character(loc.data[cond, ]$zip.zcta))
    
}
wa.fires = as.character(wild[wild$State %in% "Washington", ]$Name)
loc.data <- temp[unlist(apply(temp[, c("long", "lat")], 1, function(x) sum(!is.na(x)) == 
    2)), c("zip.zcta", "long", "lat")]
wild <- apply(wild, 1, as.list)
par(new = T)
plot(NA, ylim = c(0, 200), xlim = c(1, 185), xlab = "", ylab = "", xaxt = "n")
forplot = unlist(lapply(wild, function(x) x[x$Name %in% wa.fires]$Start.Date))
lab.forplot = unlist(lapply(wild, function(x) x[x$Name %in% wa.fires]$Name))
abline(v = match(forplot, date.levels))
text(match(forplot, date.levels), 100, lab.forplot, srt = 90, cex = 0.5)
zip.nearby <- lapply(wild, get_zip.nearby, 200)
all.wa = unique(unlist(zip.nearby[unlist(lapply(wild, function(x) x$Name %in% wa.fires))]))
wf.info = lapply(zip.nearby, function(x) length(unique(temp.zip[temp.zip$zip.zcta %in% x, 
    ]$healthCode)))
names(wf.info) = unlist(lapply(wild, function(x) x$Name))
aqi.out <- list()
zip.clust <- list()
mycut <- mycut.week
for (j in 1:length(zip.nearby)) {
    zip.clust[[j]] <- factor(temp.zip$clust.ext %in% zip.nearby[[j]])
    trig <- tapply(temp$get_worse, factor(zip.clust[[j]], levels = TRUE), as.character)
    date <- tapply(temp$date, factor(zip.clust[[j]], levels = TRUE), as.Date)
    out <- list()
    nout <- list()
    for (i in 1:length(date)) {
        out[[i]] <- get_dist(trig[[i]], date[[i]], mycut, 5)[[1]]
        nout[[i]] <- get_dist(trig[[i]], date[[i]], mycut, 5)[[2]]
    }
    aqi.out[[j]] <- t(do.call(rbind, lapply(out, function(x) x[, trig.lev1 %in% "16"])))
    colnames(aqi.out[[j]]) <- names(trig)
}
```
