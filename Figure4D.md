## Figure 4D
```markdown
pdf("check_aqi_wildfire.pdf")
clust.col <- rep("black", length(zip.clust))
for (j in 1:length(zip.clust)) {
    par(mar = c(6.1, 6.1, 8.1, 8.1))
    #### 
    check <- aqi.out[[j]][, apply(aqi.out[[j]], 2, function(x) sum(!is.na(x))) >= 5]
    n.check <- nout[apply(aqi.out[[j]], 2, function(x) sum(!is.na(x))) >= 5]
    if (length(check) > 0) {
        y = check
        forplot <- y[!is.na(y)]
        dat.mat = my.bbands(forplot, 3, 1)
        row.names(dat.mat) = names(forplot)
        matplot(match(row.names(dat.mat), date.levels), dat.mat, type = "l", ylim = c(0, 
            0.4), xlim = c(1, 185), axes = T, ylab = "", xlab = "", main = "")
        start = match(wild[[j]]$Start.Date, date.levels)
        end = ifelse(as.Date(wild[[j]]$End.Date) > "2015-09-09", 185, match(wild[[j]]$End.Date, 
            date.levels))
        rect(start, -1, end, 200, col = rgb(0.5, 0.5, 0.5, 1/4), border = rgb(0.5, 0.5, 0.5, 
            1/4))
        legend("topleft", col = c(clust.col[i]), c("PM2.5", "% Trigger Dist (Air Quality)"), 
            lty = c(2, 1), lwd = c(1, 3), cex = 0.75, bg = "white")
        axis(4, at = (0:10) * 20, lab = (0:10) * 5/(100), las = 2)
        mtext("% Trigger Dist.", side = 4, line = 2, cex = 1, las = 2)
    }
}
dev.off()
################################################################ 
temp <- temp.clust2
reg.users = tapply(temp$healthCode, temp$clust.ext, unique)
bl.order = c("Animals", "Pollen", "A cold", "Smoking", "Exercise", "House dust", "Air quality", 
    "Change in weather", "More activity", "Mold", "Strong smells", "Extreme Cold", "Other", 
    "Stress", "Extreme heat", "I don't know", "None of these")
get_out.table = function(user.set) {
    out <- lapply(YouA.data[YouA.data$healthCode %in% user.set, ]$asthma_gets_worse_with, 
        get_troubles)
    out.table <- table(unlist(out))/sum(table(unlist(out)))
    names(out.table) <- trig.lab
    other = sum(out.table[c(19, 20, 15)])
    animals = sum(out.table[7:9])
    mynew = out.table[c(1:5, 6, 10:14, 16:18, 21:22)]
    names(mynew) = trig.lab[c(1:5, 6, 10:14, 16:18, 21:22)]
    mynew = c(Animals = animals, Other = other, mynew)
    # mynew[match(bl.order,names(mynew))]
    sort(mynew, decreasing = T)
}
bl.north = get_out.table(reg.users[[1]])
bl.south = get_out.table(reg.users[[2]])
trig.lab <- c("A cold", "Exercise", "More activity", "Strong smells", "Exhuast fumes", "House dust", 
    "Dogs", "Cats", "Other furry animals", "Mold", "Pollen", "Extreme heat", "Extreme Cold", 
    "Change in weather", "Period", "Air quality", "Smoking", "Stress", "Feelings", "Laughter", 
    "I don't know", "None of these")
n <- length(trig.lab)
library("colorspace")
trig.colors <- sample(rainbow_hcl(n), n, replace = F)
trig.lev0 <- as.character(1:22)
mycut <- date.levels
trig.lev1 = trig.lev0
trig <- tapply(temp$get_worse, factor(temp$clust.ext), as.character)
date <- tapply(temp$date, factor(temp$clust.ext), as.Date)
out <- list()
nout <- list()
for (i in 1:length(date)) {
    out[[i]] <- get_dist(trig[[i]], date[[i]], mycut, 10)[[1]]
    nout[[i]] <- get_dist(trig[[i]], date[[i]], mycut, 10)[[2]]
}
get_stacks = function(x, bl.reg, mymain) {
    colnames(x) = trig.lab
    other = apply(x[, c(19, 20, 15, 5)], 1, sum)
    animals = apply(x[, 7:9], 1, sum)
    reg.ts <- cbind(Animals = animals, Other = other, x[, c(1:5, 6, 10:14, 16:18, 21:22)])
    reg.ts <- apply(reg.ts, 2, function(x) smooth.spline(na.omit(x))$y)
    reg.ts = t(reg.ts[, match(names(bl.reg), colnames(reg.ts))])
    reg.ts = cbind(bl.reg, reg.ts)
    barplot(prop.table(reg.ts, 2)[nrow(reg.ts):1, 1:133], main = mymain, col = trig.colors[match(row.names(reg.ts), 
        names(trig.colors))], border = NA, space = c(0, 10, rep(0, 104), 0.5, rep(0, 26)), 
        xaxt = "n")
    legend("topright", names(trig.colors[match(row.names(reg.ts), names(trig.colors))]), 
        fill = trig.colors[match(row.names(reg.ts), names(trig.colors))][18:1], cex = 0.8)
    
    axis(1, at = na.omit(match(mycut.month, date.levels)), na.omit(date.levels[match(mycut.month, 
        date.levels)]), las = 2)
}
```
