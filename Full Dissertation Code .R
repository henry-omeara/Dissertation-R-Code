#Kaplan-Meier Plots for Survey Results

#Install Packages for Survival and Data Extraction
install.packages("survival")
install.packages("dplyr")
install.packages("KMsurv")

library("survival")
library("dplyr")
library("KMsurv")

#Concussion
#'Concussion Data' Dataset Imported from Excel
View(Concussion_Data)
Con_Surv <- Surv(Concussion_Data$time, Concussion_Data$n.event)
Con_Survfit <- survfit(Con_Surv~1)
plot(Con_Survfit, conf.int="none", mark.time=TRUE, xlab = "Time (Years)", ylab = "Proportion Survival", 
    main = "Concussion Injuries - Kaplan-Meier Plot", las=1)
#View breakdown of Concusion Data
Con_Survfit
summary(Con_Survfit)

#Knee joint/ligament sprain
#'Knee Data' Dataset Imported from Excel
Knee_Surv <- Surv(Knee_Data$time, Knee_Data$n.event)
Knee_Survfit <- survfit(Knee_Surv~1)
plot(Knee_Survfit, conf.int="none", mark.time=TRUE, xlab = "Time (Years)", ylab = "Proportion Survival", 
    main = "Knee Joint or Ligament Injuries - Kaplan-Meier Plot", las=1)
#View breakdwon of Knee Data
Knee_Survfit
summary(Knee_Survfit)

#Ankle joint/ligament sprain
#'Ankle Data' Dataset Imported from Excel
Ankle_Surv <- Surv(Ankle_Data$time, Ankle_Data$n.event)
Ankle_Survfit <- survfit(Ankle_Surv~1)
plot(Ankle_Survfit, conf.int="none", mark.time=TRUE, xlab = "Time (Years)", ylab = "Proportion Survival", 
     main = "Ankle Joint or Ligament Injuries - Kaplan-Meier Plot", las=1)
#View breakdwon of Ankle Data
Ankle_Survfit
summary(Ankle_Survfit)

#Shoulder joint/ligament sprain
#'Shoulder Data' Dataset Imported from Excel
Shoulder_Surv <- Surv(Shoulder_Data$time, Shoulder_Data$n.event)
Shoulder_Survfit <- survfit(Shoulder_Surv~1)
plot(Shoulder_Survfit, conf.int="none", mark.time=TRUE, xlab = "Time (Years)", ylab = "Proportion Survival", 
     main = "Shoulder Joint or Ligament Injuries - Kaplan-Meier Plot", las=1)
#View breakdwon of Shoulder Data
Shoulder_Survfit
summary(Shoulder_Survfit)

#Thigh Muscle Strain
#'Thigh Data' Dataset Imported from Excel
Thigh_Surv <- Surv(Thigh_Data$time, Thigh_Data$n.event)
Thigh_Survfit <- survfit(Thigh_Surv~1)
plot(Thigh_Survfit, conf.int="none", mark.time=TRUE, xlab = "Time (Years)", ylab = "Proportion Survival", 
     main = "Thigh Muscle Strain Injuries - Kaplan-Meier Plot", las=1)
#View breakdwon of Thigh Data
Thigh_Survfit
summary(Thigh_Survfit)

---------------------------------------------------------------------------------------------------------------------------------------

#Plot Kaplan-Meier for Concussion Injuries wearing headgear vs no headgear
names(Concussion_Data_Headgear)
Con_Protection <- Surv(Concussion_Data_Headgear$time, Concussion_Data_Headgear$n.event)
Con_Protection_fit=survfit(Con_Protection ~ Concussion_Data_Headgear$Cap)
plot(Con_Protection_fit, conf.int="none", mark.time=TRUE, main = "Significance of Protective Headgear on Concussion Injuries", 
     xlab = "Time (Years)", ylab = "Proportional Survival", col=c("red","blue"), las=1)

legend("topright", inset=.01, c("Headgear","No Headgear"), 
       col = c("red", "blue"), horiz=FALSE, cex=1)

survdiff(Con_Protection ~ Concussion_Data_Headgear$Cap)

---------------------------------------------------------------------------------------------------------------------------------------

#Fit Cox Proportional Hazard Model
cox.mod <- coxph(formula=Surv(Cox_Proportional_Hazard_Model$time, Cox_Proportional_Hazard_Model$n.event)
                 ~ Concussion + Knee + Ankle + Shoulder + Thigh, data = Cox_Proportional_Hazard_Model)
cox.mod
summary(cox.mod)

#Fit Cox Proportional Hazard Model for Protective Headgear
cox.mod2 <- coxph(Surv(time, n.event) ~ Cap, data = Concussion_Data_Headgear)
cox.mod2
summary(cox.mod2)

---------------------------------------------------------------------------------------------------------------------------------------

#Data Visualisation  
#Install ggplot package to enable data visualisation techniques
install.packages("ggplot2")
library("ggplot2")

#Plot Injury Age data using boxplots
Injury_Age <- ggplot(Injury_Age, aes(Injury, Age), main = "Injury Age Breakdown")
Injury_Age + geom_boxplot(varwidth = TRUE, outlier.colour = "red", 
                          outlier.shape = 1)

#Plot No. of Forwards or Backs per Injury
InjuryPos <- ggplot(Injury_Position, aes(Position, Injury))
InjuryPos+geom_count(aes(group = Injury, color = ..n.., size = ..n..))+
  guides(color = 'legend')+scale_size_area()

#Voilin Plot to show distribution of no. of injuires across forwards and backs
#Import 'Forward Back' dataset
plot <- ggplot(Forward_Back, aes(Position, Injuries), 
               main="Number of Injuries per Position Type")
plot + geom_violin(scale = "count", aes(fill = Position), 
                   draw_quantiles = c(0.25, 0.5, 0.75))

