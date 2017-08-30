## Table 1
```markdown
get_column <- function(user) {
    temp1 <- mile.data[mile.data$healthCode %in% user, c("gender", "age", "healthCode")]  #[1] is male [2] is female
    temp1$gender = as.character(ifelse(temp1$gender == "[1]", "Male", "Female"))
    temp2 <- agesex1[agesex1$healthCode %in% user, c("NonIdentifiableDemographics.json.patientBiologicalSex", 
        "NonIdentifiableDemographics.json.patientCurrentAge", "healthCode")]
    names(temp2) <- c("gender", "age", "healthCode")
    temp2$gender = as.character(temp2$gender)
    healthCode = unique(c(as.character(temp1$healthCode), as.character(temp2$healthCode)))
    
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
```
