## Supplementary Table 2
```markdown
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
```
