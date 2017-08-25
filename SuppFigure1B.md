## Supplementary Figure 1B
```markdown
map("state", col = colors.match, fill = TRUE, resolution = 0, lty = 0.5, projection = "polyconic")
legend("topright", c("North", "South"), fill = c("blue", "red"))
trig.lev0 <- as.character(1:22)
# c('Pollen','Change.in.weather','Stress','Extreme heat','More.activity')
alt = c(11, 12, 16)
trig.lev1 = trig.lev0[alt]
trig.lab1 <- trig.lab[alt]
trig.colors1 <- trig.colors[alt]
summer.start <- "2015-06-21"
summer <- temp[as.Date(temp$date) >= summer.start, ]
summer.trigs <- tapply(summer$get_worse, summer$clust.ext, get_triggers)
summer.top.trig <- lapply(summer.trigs, get_top.trig)
spring <- temp[as.Date(temp$date) <= summer.start, ]
spring.trigs <- tapply(spring$get_worse, spring$clust.ext, get_triggers)
spring.top.trig <- lapply(spring.trigs, get_top.trig)
out <- cbind(spring.top.trig, summer.top.trig)
trig <- tapply(temp$get_worse, factor(temp$clust.ext), as.character)
date <- tapply(temp$date, factor(temp$clust.ext), as.character)
mycut.5day <- levels(cut(as.Date(date.levels), 37))
mycut.month <- c("2015-03-09", "2015-04-09", "2015-05-09", "2015-06-09", "2015-07-09", "2015-08-09", 
    "2015-09-09")
mycut.season <- c("2015-03-09", summer.start, "2015-09-9")
mycut.week <- levels(cut(as.Date(date.levels), "week"))
out <- list()
nout <- list()
for (i in 1:length(trig)) {
    out[[i]] <- get_dist(trig[[i]], date[[i]], mycut.season, 10)[[1]]
    nout[[i]] <- get_dist(trig[[i]], date[[i]], mycut.season, 10)[[2]]  #####this is the number of individuals
}
spring.out <- lapply(out, function(x) x[1, ])
spring.out <- do.call(rbind, spring.out)
summer.out <- lapply(out, function(x) x[2, ])
summer.out <- do.call(rbind, summer.out)
Season <- rep(c("spring", "summer"), nrow(spring.out))
forplot <- lapply(1:nrow(spring.out), function(x) rbind(spring.out[x, ], summer.out[x, ]))
forplot <- do.call(rbind, forplot)
trig.colors = c("green", "red", "blue")
row.names(forplot) = rep(c("Spring", "Summer"), 2)
```
