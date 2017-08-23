## Supplementary Figure 4
```markdown
user.temp <- cohorts$robust
my.puff = Dai.data[, "quick_relief_puffs"]
my.puff[is.na(my.puff)] = 0
my.puff[my.puff > 0 & my.puff < 4] = 1
my.puff[my.puff > 3] = 2
my.qr = as.character(Dai.data$use_qr)
my.qr[my.qr %in% "True"] = 1
my.qr[my.qr %in% "False"] = 0
my.qr <- as.numeric(my.qr)
try0 <- tapply(my.qr, factor(Dai.data$healthCode, levels = user.temp), as.numeric)
try1 <- tapply(my.puff, factor(Dai.data$healthCode, levels = user.temp), as.numeric)  ###puff
day <- as.character(Dai.data$day_symptoms)
day[day %in% "True"] = 1
day[day %in% "False"] = 0
day <- as.numeric(day)
try2 <- tapply(day, factor(Dai.data$healthCode, levels = user.temp), as.numeric)  ###day
night <- as.character(Dai.data$night_symptoms)
night[night %in% "True"] = 1
night[night %in% "False"] = 0
night <- as.numeric(night)
try3 <- tapply(night, factor(Dai.data$healthCode, levels = user.temp), as.numeric)  ###night
## try1 puff try2 dat try3 night try4 peakflow remove outlier
hist(Dai.data[Dai.data$peakflow < 900 & Dai.data$peakflow > 60, ]$peakflow)
peakflow = Dai.data$peakflow
peakflow[peakflow > 900 | peakflow < 60] = NA
try4 <- tapply(peakflow, factor(Dai.data$healthCode, levels = user.temp), as.numeric)
pdf("peakflow_hist.pdf")
hist(unlist(lapply(try4, sd)), main = "histogram of variance of user peakflow")
dev.off()
# try4[which(unlist(lapply(try4,sd))>100)]<-NA require more than 10 observations
get_cor <- function(x, y) {
    if (length(x) > 1) {
        temp <- cbind(x, y)
        cond <- apply(temp, 1, function(x) sum(is.na(x))) == 0
        n <- sum(cond)
        if (n > 10 & sum(x[cond] == y[cond]) != length(x[cond]) & sd(x[cond]) > 0 & sd(y[cond]) > 
            0) {
            mycor <- cor(x[cond], y[cond])
            list(mycor = mycor, n = n)
        }
    }
}
pdf("histogram_correlation.pdf", height = 10, width = 20)
par(mfrow = c(4, 4))
layout(matrix(1:16, 4, 4, byrow = TRUE))
plot.new()
out <- mapply(get_cor, try4, try2)  #peakflow versus day
## try1 puff try2 day try3 night try4 peakflow
mycor <- unlist(lapply(out, function(x) x$mycor))
mycor <- mycor[!is.na(mycor)]
nusers <- length(mycor)
n <- unlist(lapply(out, function(x) as.integer(x$n)))[!is.na(mycor)]
nobs <- mean(n)
mean = mean(mycor)
sd <- sd(mycor)
mysub <- paste("n: ", as.integer(nusers), ";  mean # observations: ", round(nobs, 1), "; mean: ", 
    round(mean, 2), "; sd:", round(sd, 2), sep = "")
hist(unlist(mycor), xlab = "cor", main = "peakflow vs. day", col = "black", breaks = 10, 
    sub = mysub, xlim = c(-1, 1))
abline(v = mean(unlist(mycor), na.rm = T), col = "red")
out <- mapply(get_cor, try4, try3)  #peakflow versus night
mycor <- unlist(lapply(out, function(x) x$mycor))
mycor <- mycor[!is.na(mycor)]
nusers <- length(mycor)
n <- unlist(lapply(out, function(x) as.integer(x$n)))[!is.na(mycor)]
nobs <- mean(n)
mean = mean(mycor)
sd <- sd(mycor)
mysub <- paste("n: ", as.integer(nusers), ";  mean # observations: ", round(nobs, 1), "; mean: ", 
    round(mean, 2), "; sd:", round(sd, 2), sep = "")
hist(unlist(mycor), col = "black", breaks = 10, xlab = "cor", main = "peakflow vs. night", 
    sub = mysub, xlim = c(-1, 1))
abline(v = mean(unlist(mycor), na.rm = T), col = "red")
out <- mapply(get_cor, try4, try0)  #peakflow versus puff
mycor <- unlist(lapply(out, function(x) x$mycor))
mycor <- unlist(lapply(out, function(x) x$mycor))
mycor <- mycor[!is.na(mycor)]
nusers <- length(mycor)
n <- unlist(lapply(out, function(x) as.integer(x$n)))[!is.na(mycor)]
nobs <- mean(n)
mean = mean(mycor)
sd <- sd(mycor)
mysub <- paste("n: ", as.integer(nusers), ";  mean # observations: ", round(nobs, 1), "; mean: ", 
    round(mean, 2), "; sd:", round(sd, 2), sep = "")
hist(unlist(mycor), col = "black", breaks = 10, xlab = "cor", main = "peakflow vs. puff", 
    sub = mysub, xlim = c(-1, 1))
abline(v = mean(unlist(mycor), na.rm = T), col = "red")
plot.new()
plot.new()
out <- mapply(get_cor, try2, try3)  #day vs night
mycor <- unlist(lapply(out, function(x) x$mycor))
mycor <- mycor[!is.na(mycor)]
nusers <- length(mycor)
n <- unlist(lapply(out, function(x) as.integer(x$n)))[!is.na(mycor)]
nobs <- mean(n)
mean = mean(mycor)
sd <- sd(mycor)
mysub <- paste("n: ", as.integer(nusers), ";  mean # observations: ", round(nobs, 1), "; mean: ", 
    round(mean, 2), "; sd:", round(sd, 2), sep = "")
hist(unlist(mycor), xlab = "cor", col = "black", breaks = 10, main = " day vs. night", sub = mysub, 
    xlim = c(-1, 1))
abline(v = mean(unlist(mycor), na.rm = T), col = "red")
out <- mapply(get_cor, try2, try0)  #day versus puff
mycor <- unlist(lapply(out, function(x) x$mycor))
mycor <- unlist(lapply(out, function(x) x$mycor))
mycor <- mycor[!is.na(mycor)]
nusers <- length(mycor)
n <- unlist(lapply(out, function(x) as.integer(x$n)))[!is.na(mycor)]
nobs <- mean(n)
mean = mean(mycor)
sd <- sd(mycor)
mysub <- paste("n: ", as.integer(nusers), ";  mean # observations: ", round(nobs, 1), "; mean: ", 
    round(mean, 2), "; sd:", round(sd, 2), sep = "")
hist(unlist(mycor), xlab = "cor", col = "black", breaks = 10, main = "day vs. puff", sub = mysub, 
    xlim = c(-1, 1))
abline(v = mean(unlist(mycor), na.rm = T), col = "red")
plot.new()
plot.new()
plot.new()
out <- mapply(get_cor, try0, try3)  #puff versu night
mycor <- unlist(lapply(out, function(x) x$mycor))
mycor <- mycor[!is.na(mycor)]
nusers <- length(mycor)
n <- unlist(lapply(out, function(x) as.integer(x$n)))[!is.na(mycor)]
nobs <- mean(n)
mean = mean(mycor)
sd <- sd(mycor)
mysub <- paste("n: ", as.integer(nusers), ";  mean # observations: ", round(nobs, 1), "; mean: ", 
    round(mean, 2), "; sd:", round(sd, 2), sep = "")
hist(unlist(mycor), xlab = "cor", col = "black", breaks = 10, main = "puff vs. night", sub = mysub, 
    xlim = c(-1, 1))
abline(v = mean(unlist(mycor), na.rm = T), col = "red")
dev.off()
```
