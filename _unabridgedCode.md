## Asthma Mobile Health Study
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
**GINA.cal** = function(user.set) {
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
# align data with map definitions by (partial) matching state,county names, which include
# multiple polygons for some counties
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
###################################################### FIGURE 1A
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
###################################################### TABLE 1
get_column <- function(user) {
    temp1 <- mile.data[mile.data$healthCode %in% user, c("gender", "age", "healthCode")]  #[1] is male [2] is female
    temp1$gender = as.character(ifelse(temp1$gender == "[1]", "Male", "Female"))
    temp2 <- agesex1[agesex1$healthCode %in% user, c("NonIdentifiableDemographics.json.patientBiologicalSex", 
        "NonIdentifiableDemographics.json.patientCurrentAge", "healthCode")]
    names(temp2) <- c("gender", "age", "healthCode")
    temp2$gender = as.character(temp2$gender)
    healthCode = unique(c(as.character(temp1$healthCode), as.character(temp2$healthCode)))
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
    age <- unlist(lapply(healthCode, get_y, "age"))
    gender <- unlist(lapply(healthCode, get_y, "gender"))
    age.class <- cut(age, c(min(age, na.rm = TRUE), 34, 64, max(age, na.rm = TRUE)), labels = c("18-34", 
        "35-64", "65+"))
    age <- table(age.class)
    gender <- table(gender)
    multirace <- levels(factor(You.data[!You.data$race %in% c("[]", "[2]", "[5]", "[1]", 
        "[3]", "[6]", "[4]", "[7]", "[7,1]", "[7,2]", "[7,3]", "[7,4]", "[7,5]", "[7,6]"), 
        ]$race))
    hispanic <- You.data$ethnicity == "[1]"  #587
    nonhispanic <- You.data$ethnicity == "[2]"  #3396
    multi <- nonhispanic & You.data$race %in% multirace  #196
    white <- You.data$race == "[5]" & nonhispanic  #2776
    black <- You.data$race == "[1]" & nonhispanic  #217
    noanswer <- You.data$race == "[7]" & nonhispanic  #42
    other <- !multi & nonhispanic & !white & !black & !noanswer  #367, including no answer
    new.race <- rep(NA, nrow(You.data))
    new.race[other] <- "other"
    new.race[multi] <- "multi"
    new.race[black] <- "black"
    new.race[hispanic] <- "hispanic"
    new.race[noanswer] <- "noanswer"
    new.race[white] <- "white"
    new.race <- factor(new.race)
    demo.data <- data.frame(You.data[You.data$healthCode %in% user, c("Income", "health_insurance", 
        "education", "healthCode", "race")], new.race = new.race[You.data$healthCode %in% 
        user])
    insur <- table(factor(demo.data$health_insurance))
    names(insur) <- c("Private", "Public", "none", "no answer")
    race = table(factor(demo.data$new.race, levels = c("black", "white", "other", "multi", 
        "hispanic")))
    education = table(factor(demo.data$education, levels = levels(factor(demo.data$education))))
    education <- c(hs.nongrad = sum(education[c("[1]", "[2]")], na.rm = T), hs.grad = as.numeric(education["[3]"]), 
        some.college = sum(education[c("[4]", "[5]")], na.rm = T), college.grad = sum(education[c("[6]", 
            "[7]")], na.rm = T))
    income = table(factor(demo.data$Income, levels = levels(factor(You.data$Income))))
    income = income[!names(income) %in% "[7]"]
    names(income) <- c("<$14,999", "$15,000-21,999", "$22,000-43,999", "$44,000-60,000", 
        ">$60,000", "I don't know")
    clinical.data <- His.data[His.data$healthCode %in% user, c("healthCode", "times_hospitalized", 
        "nights", "emergency", "emergency_times", "intubated", "symptoms", "nights", "limited activity", 
        "seen_doc", "doc_times", "oral steroids", "age_when_diagnosed", "miss_work", "hospitalized_times")]
    emerg = table(factor(clinical.data$emergency, levels = c("TRUE", "FALSE")))
    hosp = table(factor(clinical.data$times_hospitalized, levels = c("TRUE", "FALSE")))
    table(factor(demo.data$health_insurance, levels = levels(factor(You.data$health_insurance))))
    diag.class <- table(cut(clinical.data$age_when_diagnosed, c(0, 18, 24, 34, 44, 54, 64, 
        max(clinical.data$age_when_diagnosed, na.rm = TRUE))))
    diag.class <- c(`(0-18]` = as.numeric(diag.class["(0,18]"]), `19+` = sum(diag.class[!names(diag.class) %in% 
        ">18"]))
    medi.data <- data.frame(Med.data[Med.data$healthCode %in% user, c("prescribed_asthma_control_medication", 
        "daily_inhaled_medicine")])
    acpresc <- table(factor(medi.data$prescribed_asthma_control_medication))
    names(acpresc) <- c("Yes", "No", "Not sure")
    dailymed <- table(factor(medi.data$daily_inhaled_medicine))
    dailymed <- c(`ICS/LABA` = sum(dailymed[c("[1]", "[3]", "[4]", "[7]")]), ICS = sum(dailymed[c("[2]", 
        "[5]", "[6]", "[8]", "[9]", "[10]")]))
    ############# 
    gina = table(factor(GINA.cal(user), levels = c("Uncontrolled", "Partly Controlled", "Well Controlled")))
    ######### 
    count <- list(age = age, gender = gender, race = race, education = education, income = income, 
        emerg = emerg, hosp = hosp, diag.class = diag.class, acpresc = acpresc, dailymed = dailymed, 
        gina = gina)
    perc <- lapply(count, function(x) round(x/sum(x), 2))
    out <- paste(unlist(count), paste("(", unlist(perc), ")", sep = ""), sep = " ")
    names(out) <- names(unlist(count))
    out
}
get_column2 <- function(user) {
    demo.data <- data.frame(You.data[You.data$healthCode %in% user, c("health_insurance", 
        "healthCode")])
    insur = table(factor(demo.data$health_insurance))
    names(insur) <- c("Private", "Public", "none", "no answer")
    clinical.data <- His.data[His.data$healthCode %in% user, c("healthCode", "times_hospitalized", 
        "nights", "emergency", "emergency_times", "intubated", "symptoms", "nights", "limited activity", 
        "seen_doc", "doc_times", "oral steroids", "age_when_diagnosed", "miss_work", "hospitalized_times")]
    intub <- table(factor(clinical.data$intubated))
    names(intub) <- c("Yes", "No", "Not sure")
    miss_work <- table(factor(clinical.data$miss_work))
    names(miss_work) <- c("not applicable", "did not miss", "1day", "2-5day", "6-9day", "10+day")
    symp <- table(factor(clinical.data$symptoms))
    names(symp) <- c("everyday", "most", "2-3", "1-4", "less than once per month", "never")
    night <- table(factor(clinical.data$nights))
    names(night) <- c("everyday", "most", "2-3", "1-4", "less than once per month", "never")
    activity <- table(clinical.data["limited activity"])
    names(activity) <- c("everyday", "most", "2-3", "1-4", "less than once per month", "never")
    hosptimes <- table(cut(clinical.data$hospitalized_times, c(0, 1, 3, 6, 55), include.lowest = T, 
        right = FALSE))
    emergtimes <- table(cut(clinical.data$emergency_times, c(0, 1, 3, 6, 11, 50), include.lowest = T, 
        right = FALSE))
    seen_doc <- table(factor(clinical.data$seen_doc, levels = c("TRUE", "FALSE")))
    doctimes <- table(cut(clinical.data$doc_times, c(0, 1, 4, 7, 10, max(clinical.data$doc_times, 
        na.rm = t)), include.lowest = T, right = FALSE))
    steroid <- table(factor(clinical.data[, "oral steroids"]))
    names(steroid) <- c("None", "One", "Two", "Three or more")
    count <- list(insur = insur, intub = intub, miss_work = miss_work, symp = symp, night = night, 
        activity = activity, hosptimes = hosptimes, emergtimes = emergtimes, seen_doc = seen_doc, 
        doctimes = doctimes, steroid = steroid)
    perc <- lapply(count, function(x) round(x/sum(x), 2))
    out <- paste(unlist(count), paste("(", unlist(perc), ")", sep = ""), sep = " ")
    names(out) <- names(unlist(count))
    out
}
out <- list(Baseline = get_column(cohorts$baseline), Robust = get_column(cohorts$robust), 
    Milestone = get_column(cohorts$milestone))
out <- do.call(cbind, out)
xtable(out)
###################################################### SUPPLEMENTARY TABLE 2
out2 <- list(Baseline = get_column2(cohorts$baseline), Robust = get_column2(cohorts$robust), 
    Milestone = get_column2(cohorts$milestone))
out2 <- do.call(cbind, out2)
xtable(out2)
###################################################### 
user.temp <- cohorts$baseline
enrol.date <- character()
get_date = function(x) {
    substring(as.character(x), 1, 10)
}
for (i in 1:length(user.temp)) {
    edate <- unique(c(get_date(His.data[His.data$healthCode == user.temp[i], ]$createdOn), 
        get_date(MedH.data[MedH.data$healthCode == user.temp[i], ]$createdOn), get_date(Med.data[Med.data$healthCode == 
            user.temp[i], ]$createdOn), get_date(Med.data[Med.data$healthCode == user.temp[i], 
            ]$createdOn), get_date(YouA.data[YouA.data$healthCode == user.temp[i], ]$createdOn), 
        get_date(Dai.data[Dai.data$healthCode == user.temp[i], ]$date)))
    if (length(edate) > 0) {
        edate = min(as.Date(edate))
    } else {
        edate = NA
    }
    enrol.date[i] = as.character(edate)
    print(i)
}
edate.data <- data.frame(healthCode = cohorts$baseline, edate = as.Date(enrol.date, format = "%Y-%m-%d"))
mymonth.cut <- as.Date(c("2015-03-09", "2015-04-08", "2015-05-09", "2015-06-09", "2015-07-10", 
    "2015-08-10", "2015-09-10"), format = "%Y-%m-%d")
user.set <- cohorts$robust
temp <- edate.data[edate.data$healthCode %in% user.set, ]
robust.enrol <- list()
for (i in 1:6) {
    robust.enrol[[i]] <- as.character(na.omit(temp[temp$edate >= mymonth.cut[i] & temp$edate < 
        mymonth.cut[i + 1], ]$healthCode))
}
robust.GINA = list(NULL)
for (i in 1:6) {
    robust.GINA[[i]] = factor(GINA.cal(robust.enrol[[i]]), levels = c("Uncontrolled", "Partly Controlled", 
        "Well Controlled"))
}
robust.GINA.sum = matrix(NA, nrow = 6, ncol = 8)
for (i in 1:6) {
    robust.GINA.sum[i, 1] = length(robust.GINA[[i]])
    robust.GINA.sum[i, 2] = sum(is.na(robust.GINA[[i]]))
    temp = table(robust.GINA[[i]])
    robust.GINA.sum[i, c(3, 5, 7)] = temp
    robust.GINA.sum[i, c(4, 6, 8)] = round(temp/sum(temp), 2)
}
myfunc <- function(v1) {
    deparse(substitute(v1))
}
get_demo.plot <- function(x, mydata) {
    xname <- myfunc(x)
    temp <- data.frame(mydata, x)
    robust.temp = list(NULL)
    for (i in 1:6) {
        robust.temp[[i]] = factor(temp[temp$healthCode %in% robust.enrol[[i]], xname], levels = levels(x))
    }
    n <- length(levels(x)) * 2 + 2
    robust.temp.sum = matrix(NA, nrow = 6, ncol = n)
    for (i in 1:6) {
        robust.temp.sum[i, 1] = length(robust.temp[[i]])
        robust.temp.sum[i, 2] = sum(is.na(robust.temp[[i]]))
        temp = table(robust.temp[[i]])
        robust.temp.sum[i, (3:n)[3:n%%2 == 1]] = temp
        robust.temp.sum[i, (3:n)[3:n%%2 == 0]] = round(temp/sum(temp), 2)
    }
    c1 <- apply(robust.temp.sum[1:3, (3:n)[3:n%%2 == 1]], 2, sum)
    c2 <- apply(robust.temp.sum[4:6, (3:n)[3:n%%2 == 1]], 2, sum)
    first <- c1/sum(c1)
    last <- c2/sum(c2)
    library("reshape2")
    temp <- cbind(first, last)
    temp.levels <- levels(x)
    temp <- data.frame(temp.levels, temp)
    melted <- melt(temp, "temp.levels")
    names(melted)[3] <- "temp.Dist"
    names(melted)[1] <- "name"
    names(melted)[2] <- "Period"
    list(melted, rbind(c1, c2))
}
user = cohorts$robust
temp1 <- mile.data[mile.data$healthCode %in% user, c("gender", "age", "healthCode")]  #[1] is male [2] is female
temp1$gender = as.character(ifelse(temp1$gender == "[1]", "Male", "Female"))
temp2 <- agesex1[agesex1$healthCode %in% user, c("NonIdentifiableDemographics.json.patientBiologicalSex", 
    "NonIdentifiableDemographics.json.patientCurrentAge", "healthCode")]
names(temp2) <- c("gender", "age", "healthCode")
temp2$gender = as.character(temp2$gender)
healthCode = unique(c(as.character(temp1$healthCode), as.character(temp2$healthCode)))
gender <- factor(unlist(lapply(healthCode, get_y, "gender")))
mydata <- data.frame(healthCode, gender)
mydata <- mydata[mydata$healthCode %in% cohorts$baseline, ]
gender <- factor(mydata[mydata$healthCode %in% cohorts$baseline, ]$gender)
###################################################### FIGURE 2A
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
###################################################### FIGURE 2B
clinical.data <- His.data[, c("healthCode", "times_hospitalized", "emergency", "intubated", 
    "symptoms", "nights", "limited activity")]
activity <- factor(clinical.data[, "limited activity"])
out <- get_demo.plot(activity, clinical.data)
ggplot(out[[1]], aes(x = Period, y = temp.Dist, fill = name)) + geom_bar(stat = "identity", 
    position = "stack")
chisq.test(out[[2]], simulate.p.value = T)
###################################################### FIGURE 2C
symptoms <- factor(clinical.data$symptoms)
out <- get_demo.plot(symptoms, clinical.data)
ggplot(out[[1]], aes(x = Period, y = temp.Dist, fill = name)) + geom_bar(stat = "identity", 
    position = "stack")
chisq.test(out[[2]], simulate.p.value = T)
###################################################### FIGURE 2D,E See: userretentionfigures_manuscript-jan2017.html SUPPLEMENTARY TABLE 3B
rob.date = edate.data[match(cohorts$robust, edate.data$healthCode), ]$edate
seq.day = seq(from = as.Date(range(rob.date)[1]), to = as.Date(range(rob.date)[2]), by = "day")
rob.ind = match(rob.date, seq.day)
rob.gina = GINA.cal(cohorts$robust)
cf1 = rob.gina
y = ifelse(cf1 == "Uncontrolled", 1, 0)
x = rob.ind
fit = glm(factor(y) ~ x, family = binomial())
summary(fit)
confint(fit, parm = "x")
exp(coef(fit)["x"])
exp(confint(fit, parm = "x"))
cdplot(factor(y) ~ x, ylab = "Well Controlled", xlab = "Enrollment Date")
rob.date = edate.data[match(cohorts$robust, edate.data$healthCode), ]$edate
seq.day = seq(from = as.Date(range(rob.date)[1]), to = as.Date(range(rob.date)[2]), by = "day")
rob.ind = match(rob.date, seq.day)
rob.gina = GINA.cal(cohorts$robust)
cf2 = gender[match(cohorts$robust, healthCode)]
x = rob.ind[!is.na(cf2)]
y = ifelse(cf2[!is.na(cf2)] == "Female", 1, 0)
fit = glm(factor(y) ~ x, family = binomial())
summary(fit)
confint(fit, parm = "x")
exp(coef(fit)["x"])
exp(confint(fit, parm = "x"))
cdplot(factor(y) ~ x, ylab = "Gender", xlab = "Enrollment Date")
### use negative version opposite coding
cf3 = clinical.data[match(cohorts$robust, clinical.data$healthCode), "limited activity"]
cf3 = as.numeric(factor(cf3))
y = cf3[!is.na(cf3)]
x = rob.ind[!is.na(cf3)]
fit = lm(y ~ x)
summary(fit)
length(y)
coef(fit)["x"]
confint(fit, parm = "x")
####### 
cf4 = clinical.data[match(cohorts$robust, clinical.data$healthCode), "symptoms"]
cf4 = as.numeric(factor(cf4))
y = cf4[!is.na(cf4)]
x = rob.ind[!is.na(cf4)]
fit = lm(y ~ x)
length(y)
summary(fit)
coef(fit)["x"]
confint(fit, parm = "x")
###################################################### FIGURE 3A
temp = Dai.data[Dai.data$healthCode %in% cohorts$robust, ]
day <- unlist(tapply(factor(temp$day_symptoms, levels = c("True", "False")), factor(temp$healthCode), 
    function(x) table(x)[[1]]/sum(table(x))))
sum(tapply(day, factor(GINA.cal(names(day))), length))
kruskal.test(day, factor(GINA.cal(names(day))))
boxplot(as.numeric(day) ~ factor(GINA.cal(names(day)), levels = c("Uncontrolled", "Partly Controlled", 
    "Well Controlled")))
###################################################### FIGURE 3B
night <- unlist(tapply(factor(temp$night_symptoms, levels = c("True", "False")), factor(temp$healthCode), 
    function(x) table(x)[[1]]/sum(table(x))))
sum(tapply(night, factor(GINA.cal(names(night))), length))
kruskal.test(night, factor(GINA.cal(names(night))))
boxplot(as.numeric(night) ~ factor(GINA.cal(names(night)), levels = c("Uncontrolled", "Partly Controlled", 
    "Well Controlled")))
###################################################### FIGURE 3C
qr.use <- unlist(tapply(factor(temp$use_qr, levels = c("True", "False")), factor(temp$healthCode), 
    function(x) table(x)[[1]]/sum(table(x))))
table(is.na(qr.use))
sum(tapply(qr.use, factor(GINA.cal(names(qr.use))), length))
kruskal.test(qr.use, factor(GINA.cal(names(qr.use))))
boxplot(as.numeric(qr.use) ~ factor(GINA.cal(names(qr.use)), levels = c("Uncontrolled", "Partly Controlled", 
    "Well Controlled")))
###################################################### FIGURE 3D
medi <- read.table("Daily_medicine_ans_by_date_last_response.tsv", sep = "\t", header = T)
medi <- medi[medi$healthCode %in% cohorts$robust, ]
medi$medicine <- factor(medi$medicine, levels <- c(1, 2, 3))
Dai.data$medicine = medi[match(Dai.data$healthCode, medi$healthCode), ]$medicine
comp2 <- unlist(tapply(factor(medi$medicine), factor(medi$healthCode), function(x) (table(x)[[1]] + 
    table(x)[[2]])/sum(table(x))))
sum(tapply(comp2, factor(GINA.cal(names(comp2))), length))
kruskal.test(comp2, factor(GINA.cal(names(comp2))))
boxplot(as.numeric(comp2) ~ factor(GINA.cal(names(comp2)), levels = c("Uncontrolled", "Partly Controlled", 
    "Well Controlled")))
###################################################### FIGURE 3E
dai.peakflow = unlist(tapply(Dai.data$peakflow, Dai.data$healthCode, mean, na.rm = T))
# cond=unlist(tapply(Dai.data$peakflow,Dai.data$healthCode,length))>2
nicole = data.frame(healthCode = cohorts$robust, GINA = GINA.cal(cohorts$robust), BiologicalSex = agesex1[match(cohorts$robust, 
    agesex1$healthCode), ]$NonIdentifiableDemographics.json.patientBiologicalSex, HeightInches = agesex1[match(cohorts$robust, 
    agesex1$healthCode), ]$NonIdentifiableDemographics.json.patientHeightInches, WeightPounds = agesex1[match(cohorts$robust, 
    agesex1$healthCode), ]$NonIdentifiableDemographics.json.patientWeightPounds, CurrentAge = agesex1[match(cohorts$robust, 
    agesex1$healthCode), ]$NonIdentifiableDemographics.json.patientCurrentAge, peakflow = unlist(dai.peakflow[match(cohorts$robust, 
    names(dai.peakflow))]))
table(is.na(nicole$peakflow))
# # load('from_marcus_091316/robust_users_data.Rdata') pfs_gina <-
# robust_users_daily_summary; rm(robust_users_daily_summary) pfs_gina <- pfs_gina[,
# !names(pfs_gina) %in% 'GINA'] colnames(pfs_gina)[2] <- 'GINA'
# marcus=pfs_gina[,match(names(nicole),names(pfs_gina))]
pfs_gina = nicole
# Clean bad data out
pfs_gina$HeightInches[ifelse(is.na(pfs_gina$HeightInches < 50), FALSE, pfs_gina$HeightInches < 
    50)] <- NA
pfs_gina$HeightInches[ifelse(is.na(pfs_gina$HeightInches > 150), FALSE, pfs_gina$HeightInches > 
    150)] <- NA
pfs_gina$WeightPounds[ifelse(is.na(pfs_gina$WeightPounds < 50), FALSE, pfs_gina$WeightPounds < 
    50)] <- NA
pfs_gina$WeightPounds[ifelse(is.na(pfs_gina$WeightPounds > 350), FALSE, pfs_gina$WeightPounds > 
    350)] <- NA
pfs_gina$peakflow[ifelse(is.na(pfs_gina$peakflow < 60), FALSE, pfs_gina$peakflow < 60)] <- NA
pfs_gina$peakflow[ifelse(is.na(pfs_gina$peakflow > 900), FALSE, pfs_gina$peakflow > 900)] <- NA
pfs_gina[complete.cases(pfs_gina[, c("GINA", "peakflow", "WeightPounds", "BiologicalSex", 
    "HeightInches", "CurrentAge")]), "healthCode"]
write.table(pfs_gina[complete.cases(pfs_gina[, c("GINA", "peakflow", "WeightPounds", "BiologicalSex", 
    "HeightInches", "CurrentAge")]), "healthCode"], file = "healthCodes4PEFregression.tsv", 
    quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
## ! constrain to only uncontrolled and partially controlled
pfs_gina <- pfs_gina[pfs_gina$GINA != "Well Controlled", ]
fit <- lm(peakflow ~ GINA + WeightPounds + BiologicalSex + HeightInches + CurrentAge, data = pfs_gina)
summary(fit)
fit <- lm(peakflow ~ GINA + BiologicalSex + HeightInches, data = pfs_gina)
summary(fit)
table(apply(pfs_gina[, c(2, 3, 4, 7)], 1, function(x) sum(is.na(x)) == 0))
## Generate smoothed curve for Peak Flow versus Height w/in stratified groups
range.height = range(pfs_gina$HeightInches, na.rm = T)
sim = data.frame(matrix(ncol = 4, nrow = (range.height[2] - range.height[1] + 1) * 2 * 2 * 
    50))
colnames(sim) <- c("Height", "Sex", "GINA", "predicted_PF")
sim$Height = rep(seq(from = range.height[1], to = range.height[2], by = 1), 4 * 50)
sim$Sex = rep(c(0, 1), (range.height[2] - range.height[1] + 1) * 50)
sim$GINA = c(rep(0, (range.height[2] - range.height[1] + 1) * 2 * 50), rep(1, (range.height[2] - 
    range.height[1] + 1) * 2 * 50))
# sim <- transform(sim, predicted_PF = 86.48 - fit$coeff[[2]] * GINA + fit$coeff[[3]]*Sex
# + fit$coeff[[4]]* Height)
sim <- transform(sim, predicted_PF = fit$coeff[[1]] - fit$coeff[[2]] * GINA + fit$coeff[[3]] * 
    Sex + fit$coeff[[4]] * Height)
sim$GINA = ifelse(sim$GINA == 0, "Uncontrolled", "Partly Controlled")
sim$Sex = ifelse(sim$Sex == 0, "Female", "Male")
# pf_data=pfs_gina[!is.na(pfs_gina$peakflow), c(1:2, 6, 8:9)]
pf_data = pfs_gina[!is.na(pfs_gina$peakflow), ]
pf_data <- pf_data[complete.cases(pf_data), ]
# colnames(pf_data)[4] <- 'Sex'
colnames(pf_data)[3] <- "Sex"
ggplot(sim, aes(x = Height, y = predicted_PF, colour = GINA, linetype = Sex)) + geom_line(size = 1.25) + 
    geom_point(data = pf_data, aes(x = HeightInches, y = peakflow, colour = GINA, linetype = Sex, 
        shape = Sex)) + labs(x = "Height (inches)", y = "Predicted Peak Flow (L/min)") + 
    theme_minimal()
###################################################### SUPPLEMENTARY FIGURE 4
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
###################################################### FIGURE 4 see: triggeranalysis.R FIGURE 5A
WE.day.index = unlist(lapply(week.data[, "date"], my.convert.day))
user.set <- cohorts$robust
Last.week = matrix(NA, nrow = length(user.set), ncol = ncol(week.data) + 1)
First.week = matrix(NA, nrow = length(user.set), ncol = ncol(week.data) + 1)
for (i in 1:length(user.set)) {
    cur.user = user.set[i]
    pick = which(week.data[, "healthCode"] == cur.user)
    if (length(pick) > 0) {
        if (length(pick) == 1) {
            Last.week[i, ] = c(WE.day.index[pick], as.matrix(week.data)[pick, ])
            First.week[i, ] = Last.week[i, ]
        } else {
            temp = week.data[pick, ]
            temp1 = WE.day.index[pick]
            Last.week[i, ] = c(max(temp1), as.matrix(temp)[which.max(temp1)[1], ])
            First.week[i, ] = c(min(temp1), as.matrix(temp)[which.min(temp1)[1], ])
        }
    }
}
colnames(Last.week) = c("Day", colnames(week.data))
colnames(First.week) = c("Day", colnames(week.data))
Last.w.day = as.numeric(Last.week[, 1])
First.w.day = as.numeric(First.week[, 1])
Len.w = Last.w.day - First.w.day
Len.w[is.na(Len.w)] = 0
hist(Len.w[Len.w > 0], xlab = "# of days betwen the first and last weekly survey", main = "")
a = 90
Last.week.3m = Last.week[Len.w > a, ]
First.week.3m = First.week[Len.w > a, ]
user.3m = user.set[Len.w > a]
col.n = c("asthma_doc_visit", "emergency_room", "prednisone", "limitations", "missed_work", 
    "admission")
p.list = rep(NA, length(col.n))
for (k in 1:length(col.n)) {
    cur.n = col.n[k]
    for (i in 1:nrow(Last.week)) {
        cur = Last.week[i, cur.n]
        if (!is.na(cur)) {
            Last.week[i, cur.n] = gsub("\\ ", "", cur)
        }
        cur = First.week[i, cur.n]
        if (!is.na(cur)) {
            First.week[i, cur.n] = gsub("\\ ", "", cur)
        }
    }
    x.v = Last.week[, cur.n] == "True"
    x.v = x.v + 0
    y.v = First.week[, cur.n] == "True"
    y.v = y.v + 0
    temp = wilcox.test(x.v, y.v, paired = TRUE)
    p.list[k] = temp$p.value
}
round(p.list, 3)
length(user.3m)
cur.n = "limitations"
table(Last.week = factor(Last.week.3m[, cur.n], levels = c("True", "False")), First.week = factor(First.week.3m[, 
    cur.n], levels = c("True", "False")))
wilcox.test(as.numeric(factor(Last.week.3m[, cur.n], levels = c("True", "False"))), as.numeric(factor(First.week.3m[, 
    cur.n], levels = c("True", "False"))), paired = T)
table(Last.week = factor(Last.week[, cur.n], levels = c("True", "False")), First.week = factor(First.week[, 
    cur.n], levels = c("True", "False")))
wilcox.test(as.numeric(factor(Last.week[, cur.n], levels = c("True", "False"))), as.numeric(factor(First.week[, 
    cur.n], levels = c("True", "False"))), paired = T)

###################################################### FIGURE 5B
GINA.m = matrix(NA, nrow = nrow(mile.data), ncol = 4)
colnames(GINA.m) = c("DaySym", "NighSym", "Reliever", "Activity")
for (i in 1:nrow(mile.data)) {
    GINA.m[i, 1] = is.element(mile.data[i, "symptoms2"], c("[1]", "[2]", "[3]")) + 0  ### day sym more than twice a week
    GINA.m[i, 2] = !is.element(mile.data[i, "nights2"], c("", "[5]", "[6]")) + 0  ### any night sym
    GINA.m[i, 3] = is.element(mile.data[i, "past_month_quick_relief2"], c("[1]", "[2]", "[3]")) + 
        0  ### more than twice a week
    GINA.m[i, 4] = !is.element(mile.data[i, "limited.activity2"], c("", "[5]", "[6]")) + 
        0  ### any limited activity
}
GINA.v = apply(GINA.m, 1, sum)
GINA.indi = GINA.v
GINA.indi[GINA.v > 2] = "Uncontrolled"
GINA.indi[GINA.v > 0 & GINA.v < 3] = "Partly Controlled"
GINA.indi[GINA.v == 0] = "Well Controlled"
table(GINA.indi)
Mile.GINA = data.frame(healthCode = mile.data[, "healthCode"], GINA.indi)
get_cont.table = function(user.temp) {
    table(GINA.cal(user.temp), Mile.GINA[match(user.temp, Mile.GINA$healthCode), ]$GINA.indi)
}
get_cont.table(cohorts$milestone)
get_cont.table(cohorts$robust)
get_cont.table(cohorts$baseline)
###################################################### FIGURE 5C table(Mile.data[,'alleviate_troubles'])
table(Mile.data[, "met_goal"])
table(Feed.data[Feed.data$healthCode %in% cohorts$milestone, "manage_asthma"])  ###168 users
###################################################### SUPPLEMENTARY TABLE 5d,e [1], [2], [3] Yes, no, don't know
a = table(factor(GINA.cal(cohorts$milestone), levels = c("Well Controlled", "Partly Controlled", 
    "Uncontrolled")), Mile.data[match(cohorts$milestone, Mile.data$healthCode), "prevent_ed"])
a
a/apply(a, 1, sum)
a = table(factor(GINA.cal(cohorts$milestone), levels = c("Well Controlled", "Partly Controlled", 
    "Uncontrolled")), Mile.data[match(cohorts$milestone, Mile.data$healthCode), "prevent_visit"])
a
a/apply(a, 1, sum)
###################################################### SUPPLEMENTARY TABLE 3A
edate.data <- data.frame(healthCode = cohorts$baseline, edate = as.Date(enrol.date, format = "%Y-%m-%d"))
mymonth.cut <- as.Date(c("2015-03-09", "2015-04-08", "2015-05-09", "2015-06-09", "2015-07-10", 
    "2015-08-10", "2015-09-10"), format = "%Y-%m-%d")
ecount.daily = table(cut(as.Date(Dai.data$date), mymonth.cut))
ecount.weekly = table(cut(as.Date(week.data$date), mymonth.cut))
get_ecount = function(x) {
    table((cut(as.Date(edate.data[edate.data$healthCode %in% x, ]$edate), mymonth.cut)))
}
ecount.cohort = do.call(cbind, lapply(cohorts, get_ecount))
########################## 
enrol.m = enrollment[, 2:5]
enrol.m = as.matrix(enrol.m)
enrol.m = matrix(as.numeric(as.vector(enrol.m)), nrow = nrow(enrol.m))
enrol.s = matrix(NA, nrow = 6, ncol = 2)
colnames(enrol.s) = c("Downloads", "Enrolled")
month.cut = 1:6
month.cut[1] = 0
month.cut[2] = my.convert.day("2015-04-08")
month.cut[3] = my.convert.day("2015-05-09")
month.cut[4] = my.convert.day("2015-06-09")
month.cut[5] = my.convert.day("2015-07-10")
month.cut[6] = my.convert.day("2015-08-10")
month.cut[7] = my.convert.day("2015-09-10")
for (i in 1:6) {
    pick = enrol.m[, 1] == i
    enrol.s[i, 1] = sum(enrol.m[pick, 4], na.rm = T)
    temp = max(enrol.m[pick, 3], na.rm = T)
    temp1 = 0
    if (i > 1) {
        temp1 = max(enrol.m[enrol.m[, 1] == (i - 1), 3], na.rm = T)
    }
    enrol.s[i, 2] = temp - temp1
    
}
enrol.s1 = cbind(downloads = enrol.s[, 1], daily = ecount.daily, daily.dist = round(ecount.daily/sum(ecount.daily), 
    2), weekly = ecount.weekly, weekly.dist = round(ecount.weekly/sum(ecount.weekly), 2), 
    enrolled = enrol.s[, 2], enrol.perc = round(enrol.s[, 2]/enrol.s[, 1], 2), baseline = ecount.cohort[, 
        1], baseline.perc = round(ecount.cohort[, 1]/enrol.s[, 1], 2), robust = ecount.cohort[, 
        2], robust.perc = round(ecount.cohort[, 2]/enrol.s[, 1], 2), milestone = ecount.cohort[, 
        3], milestone.perc = round(ecount.cohort[, 3]/enrol.s[, 1], 2))
```
