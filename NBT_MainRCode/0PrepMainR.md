## Preparation Code for NBT Main R Code
```markdown
library(ggplot2)
library(reshape2)
library(gplots)
library(maps)
library(mapproj)
library(xtable)
library(wordcloud)
library(colorspace)

########## FUNCTION DEFINITIONS
mon0 = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

get_y <- function(x, var) {
    y <- temp2[temp2$healthCode %in% x, var]
    if (length(y) == 0) {
        z = temp1[temp1$healthCode %in% x, var]
    } else {
        if (is.na(y)) {
            z = ifelse(length(temp1[temp1$healthCode %in% x, var]) > 0, temp1[temp1$healthCode %in% 
                x, var], y)
        } else {
            z = y
        }
    }
}

my.convert.day <- function(date.v, mon.index = mon0, year.c = year0) {
    ##### day 0: 2015-3-9
    year = as.numeric(substring(date.v, 1, 4))
    mon = as.numeric(substring(date.v, 6, 7))
    day = as.numeric(substring(date.v, 9, 10))
    count = 0
    if (year - 2015 > 0) {
        count = (year - 2015) * year.c - 68
        if (mon > 1) 
            count = count + sum(mon.index[1:(mon - 1)])
        count = count + day
    } else {
        if (mon > 3) {
            count = count + sum(mon.index[3:(mon - 1)]) - 9
            count = count + day
        } else {
            count = count + day - 9
        }
    }
    return(count)
}

##### Calculate whether user meets GINA standards
GINA.cal = function(user.set) {
    GINA.matrix = matrix(NA, nrow = length(user.set), ncol = 4)
    colnames(GINA.matrix) = c("DaySym", "NighSym", "Reliever", "Activity")
    
    for (i in 1:length(user.set)) {
        cur.user = user.set[i]
        #### handle Quick reliver
        cur.pick = which(Med.data[, "healthCode"] == cur.user)
        if (length(cur.pick) != 0) {
            a = max(cur.pick)
            Quick.rel = Med.data[a, "past_month_quick_relief"]
            GINA.matrix[i, 3] = 0
            #### more than twice a week
            if (!is.element(Quick.rel, c("", "[5]", "[4]"))) 
                GINA.matrix[i, 3] = 1
        }
        #### handle other three
        cur.pick = which(His.data[, "healthCode"] == cur.user)
        if (length(cur.pick) != 0) {
            a = max(cur.pick)
            Day.sym = His.data[a, "symptoms"]
            GINA.matrix[i, 1] = 0
            if (!is.element(Day.sym, c("", "[5]", "[4]", "[6]"))) 
                #### day symptom more than twice a week
            GINA.matrix[i, 1] = 1
            
            Nig.sym = His.data[a, "nights"]
            GINA.matrix[i, 2] = 0
            if (!is.element(Nig.sym, c("", "[5]", "[6]"))) 
                #### any night symptom
            GINA.matrix[i, 2] = 1
            
            Act.sym = His.data[a, "limited activity"]
            GINA.matrix[i, 4] = 0
            if (!is.element(Day.sym, c("", "[5]", "[6]"))) 
                #### any limited activity
            GINA.matrix[i, 4] = 1
        }
    }
    GINA.v = apply(GINA.matrix, 1, sum)
    GINA.indi = GINA.v
    GINA.indi[GINA.v > 2] = "Uncontrolled"
    GINA.indi[GINA.v > 0 & GINA.v < 3] = "Partly Controlled"
    GINA.indi[GINA.v == 0] = "Well Controlled"
    return(GINA.indi)
}

###################################################### LOAD DATA
Dai.data <- read.table("Dai_data.txt", header = TRUE, sep = "\t", check.names = FALSE)  
##see: amha_data_preprocessing_manuscript_Jan2017.html

week.data <- read.table("week_data.txt", header = TRUE, sep = "\t", check.names = FALSE)  
##see: amha_data_preprocessing_manuscript_Jan2017.html

mile.data <- read.table("mile_data.txt", header = TRUE, sep = "\t", check.names = FALSE)
You.data <- read.table("You_data.txt", header = TRUE, sep = "\t", check.names = FALSE)
His.data <- read.table("His_data.txt", header = TRUE, sep = "\t", check.names = FALSE)
MedH.data <- read.table("MedH_data.txt", header = TRUE, sep = "\t", check.names = FALSE)
Med.data <- read.table("Med_data.txt", header = TRUE, sep = "\t", check.names = FALSE)
YouA.data <- read.table("YouA_data.txt", header = TRUE, sep = "\t", check.names = FALSE)
agesex1 = read.table("agesex1.txt", header = TRUE, sep = "\t", check.names = FALSE)
Feed.data = read.table("Feed_data.txt", header = TRUE, sep = "\t", check.names = FALSE)
cdc.prev = read.table("cdc_prev.txt", header = TRUE, sep = "\t", check.names = FALSE)
enrollment = read.table("enrollment.txt", header = TRUE, sep = "\t", check.names = FALSE)

############### define cohorts (recursive)
baseline = as.character(You.data[, "healthCode"])
baseline = c(baseline, as.character(His.data[, "healthCode"]))
baseline = c(baseline, as.character(MedH.data[, "healthCode"]))
baseline = c(baseline, as.character(Med.data[, "healthCode"]))
baseline = c(baseline, as.character(YouA.data[, "healthCode"]))
baseline = c(baseline, as.character(Dai.data[, "healthCode"]))
baseline = unique(baseline)

length(baseline)  #6,470

milestone.314 <- baseline[baseline %in% mile.data$healthCode]
length(milestone.314)

Mile.data = mile.data[mile.data$healthCode %in% milestone.314, ]

############ robust user definition
temp = apply(MedH.data[, c("Congestive", "chronic_pulmonary_diesease", "other_lung_disease")], 
    1, sum, na.rm = T)
user.nocompete = as.character(unique(MedH.data[temp == 0, "healthCode"]))
length(unique(MedH.data[temp != 0, "healthCode"]))
pack.year = You.data[, "avg_cigarettes"]/20 * You.data[, "smoking_years"]

temp = You.data[, "smoking_status"] == "[1]"
temp1 = pack.year < 10
user.nonsmoke = as.character(You.data[temp | temp1, "healthCode"])
user.set0 <- as.character(intersect(user.nonsmoke, user.nocompete))

temp <- list(Dai.users = Dai.data[, "healthCode"], week.users = week.data[, "healthCode"])
user.five <- as.character(names(table(unlist(temp)))[table(unlist(temp)) >= 5])
for_venn <- list(user.nonsmoke = user.nonsmoke, user.nocompetingrisk = user.nocompete, user.five = user.five)
venn(for_venn)
user.set = names(table(unlist(for_venn)))[table(unlist(for_venn)) == 3]
cohorts <- list(baseline = baseline, robust = user.set, milestone = milestone.314)
lapply(cohorts, length)

###################################################### FIGURE 1
map("state", interior = FALSE)
map("state", boundary = FALSE, col = "gray", add = TRUE)

#align data with map definitions by (partial) matching state,county names, which include
#multiple polygons for some counties

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

##### pre-processed file for the protection of exact user locations
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

#### draw map
map("state", col = colors[colorsmatched], fill = TRUE, resolution = 0, lty = 0, projection = "polyconic")
title("All Users")
legend("topright", leg.txt, horiz = TRUE, fill = colors, cex = 0.6)

```
