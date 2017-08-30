## Figure 3E
```markdown
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
```
