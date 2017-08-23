## Figure 1
```markdown
map("state", interior = FALSE)
map("state", boundary = FALSE, col = "gray", add = TRUE)
# align data with map definitions by (partial) matching state,county names, which include multiple polygons for some counties
my.state.name = state.name
for (i in 1:length(state.name)) {
    name = state.name[i]
    my.state.name[i] = paste(tolower(substr(name, 1, nchar(name))), sep = "")
}
temp = state.abb[match(map("state", plot = FALSE)$names, my.state.name)]
temp[34:37] = "NY"
temp[38:40] = "NC"
temp[53:55] = "VA"
temp[56:60] = "WA"
temp[20:22] = "MA"
temp[23:24] = "MI"
temp[8] = "DC"
mapstate <- temp
### we provide pre-processed file to protect exact user locations
bl.loc <- read.table("bl_loc.txt", header = TRUE, sep = "\t")
all.state = bl.loc$state
user.temp <- cohorts$baseline[!is.na(all.state)]
mytable <- table(bl.loc$state)
sum(mytable)
app.state.prev <- mytable/sum(mytable)
my.plot = list(NULL)
my.plot$percent = mytable[match(mapstate, names(mytable))]
colors = c("#F1EEF6", "#D4B9DA", "#C994C7", "#DF65B0", "#DD1C77", "#980043")
my.plot$colorBuckets <- as.numeric(cut(my.plot$percent, c(0, 10, 20, 50, 100, 300, 1000)))
leg.txt <- c("1-10", "10-19", "20-49", "50-99", "100-300", ">300")
colorsmatched <- my.plot$colorBuckets
# draw map
map("state", col = colors[colorsmatched], fill = TRUE, resolution = 0, lty = 0, projection = "polyconic")
title("All Users")
legend("topright", leg.txt, horiz = TRUE, fill = colors, cex = 0.6)
```
