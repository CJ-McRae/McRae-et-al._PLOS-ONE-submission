#_________________________________________________________________________________________#
#PLOS ONE manuscript submission

#"Seasonal dynamics of algal symbionts and holobiont lipidome in corals from reefs with 
#distinct thermal regimes"

#McRae et al. 
#submitted May 2022
#Revised July 2022

#Contact information: 
#Dr. Crystal J. McRae 
#crystal.j.mcrae@gmail.com


#Species abbreviations:
#PA = Pocillopora acuta
#AN = Acropora nana
#PL = Porites lutea

#Site abbreviations:
#WT = Wanlitung reef
#OT = Outlet reef
#_________________________________________________________________________________________#


#set language to English
Sys.setenv(LANG = "en")

####___________####

####<TEMPERATURE>####

####1) Libraries####

library(tidyverse)
library(lubridate)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)

####2) Data####

raw_temp <- read_csv("TEMP.csv")


#class check & organize the data

#Separate month, day, hour, and minute
TEMPDATA <-
  raw_temp %>%
  mutate(Date_Time = mdy_hm(raw_temp$DATETIME)) %>% 
  mutate(year= year(Date_Time), 
         month = month(Date_Time),
         day = day(Date_Time), 
         hour = hour(Date_Time), 
         minute = minute(Date_Time))

#Define seasons
DATA <-
  TEMPDATA%>%
  mutate(month_conversion = ifelse(year== 2018, -5, 7))%>%
  mutate(MONTHID = month + month_conversion)%>%
  mutate(
    season = case_when(
      MONTHID %in% 10:12 ~ "spring",
      MONTHID %in%  4:6  ~ "fall",
      MONTHID %in%  7:9  ~ "winter",
      MONTHID %in%  13:15  ~ "ssummer",
      TRUE ~ "summer"))


#create separate date column for plotting
DATA <- tidyr::separate(DATA, 'DATETIME',
                        into = c('date', 'time'),
                        sep= ' ')


DATA$date <- as.Date (DATA$date,"%m/%d/%Y")
DATA$SITE <- as.factor (DATA$SITE)
DATA$month <- as.integer (DATA$month)
DATA$MONTHID <- as.integer (DATA$MONTHID)
DATA$season <- as.factor (DATA$season)

str(DATA)
summary(DATA)
head(DATA)


#Monthly summaries

month_sum <-
  DATA%>%
  group_by(month, SITE, year)%>%
  summarise(TEMP_mean = mean(TEMP), TEMP_sd =sd(TEMP))

month_var <-
  DATA%>%
  group_by(month, SITE, year)%>%
  summarise(TEMP_max = max(TEMP), TEMP_min =min(TEMP))


#Daily summaries

day_sum <-
  DATA%>%
  group_by(date, SITE, month, year, season, MONTHID)%>%
  summarise(TEMP_mean = mean(TEMP), TEMP_sd =sd(TEMP), TEMP_max=max(TEMP), TEMP_min = min(TEMP))%>%
  mutate(day_range = TEMP_max - TEMP_min)

#Average of daily values
day_sum_short<-
  day_sum%>%
  group_by(month, year, SITE, season, MONTHID)%>%
  summarise(day_range = mean(day_range), meanday_max =mean (TEMP_max), meanday_min = mean(TEMP_min), meanday = mean(TEMP_mean), meanday_sd = mean(TEMP_sd))




####3) Plots####

#first make subsets for each site

OT_TEMP <-
  DATA%>%
  filter(SITE =="OT")

head(OT_TEMP)
tail(OT_TEMP)

WT_TEMP <-
  DATA%>%
  filter(SITE =="WT")

head(WT_TEMP)
tail(WT_TEMP)



####*Outlet_10min data####

OT_plot <- ggplot(data=OT_TEMP, 
                  aes(x=date, y=TEMP)) +
  geom_line(size=0.05, colour='darkgrey')+
  theme_classic()+
  theme(text=element_text(size=14,  family="serif"))+
  geom_hline(yintercept=30, linetype="dashed", 
             color = "black", size=0.5)+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  scale_y_continuous(limits=c(22, 34), breaks=c(22, 24, 26, 28, 30, 32, 34))+
  labs(title="Outlet reef (thermally variable site)", y="Temperature (?C)", x="Month")

OT_plot 


####*Wanlitung_10min data####

WT_plot <- ggplot(data=WT_TEMP, 
                  aes(x=date, y=TEMP)) +
  geom_line(size=0.05, colour='steelblue')+
  theme_classic()+
  theme(text=element_text(size=14,  family="serif"))+
  geom_hline(yintercept=30, linetype="dashed", 
             color = "black", size=0.5)+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  scale_y_continuous(limits=c(22, 34), breaks=c(22, 24, 26, 28, 30, 32, 34))+
  labs(title="Wanlitung reef (thermally stable site)", y="Temperature (?C)", x="Month")

WT_plot 



####*daily mean####
daymean_plot <- 
  ggplot(data=day_sum, aes(x=date, y=TEMP_mean, colour=SITE)) +
  geom_smooth()+
  theme_bw()+
  scale_color_manual(values=c("darkgrey", "steelblue")) + theme_classic()+
  theme(axis.text.x = element_text(angle=0, margin = margin(t=5, r=50)))+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  labs(title="Mean daily temperature", y="Temperature (?C)", x="")+
  theme(text=element_text(size=14,  family="serif"))

daymean_plot +   theme(legend.position = "none")


#####*daily range####

daymean_range_plot <- 
  ggplot(data=day_sum, aes(x=date, y=day_range, colour=SITE)) +
  geom_smooth()+
  theme_bw()+
  scale_color_manual(values=c("darkgrey", "steelblue")) + theme_classic()+
  theme(axis.text.x = element_text(angle=0, margin = margin(t=5, r=50)))+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  labs(title="Daily temperature range", y="Temperature (?C)", x="")+
  theme(text=element_text(size=14,  family="serif"))

daymean_range_plot  + theme(legend.position = "none")


#####*mean daily_max####
daymean_max_plot <- 
  ggplot(data=day_sum, aes(x=date, y=TEMP_max, colour=SITE)) +
  geom_smooth()+
  theme_bw()+
  scale_y_continuous(limits=c(21, 34), breaks=c(24, 28, 32))+
  scale_color_manual(values=c("darkgrey", "steelblue")) + theme_classic()+
  theme(axis.text.x = element_text(angle=0, margin = margin(t=5, r=50)))+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  labs(title="Daily temperature maxima", y="Temperature (?C)", x="")+
  theme(text=element_text(size=14,  family="serif"))

daymean_max_plot + theme(legend.position = "none")



####*mean daily_min####
daymean_min_plot <- 
  ggplot(data=day_sum, aes(x=date, y=TEMP_min, colour=SITE)) +
  geom_smooth()+
  theme_bw()+
  scale_y_continuous(limits=c(21, 34), breaks=c(24, 28, 32))+
  scale_color_manual(values=c("darkgrey", "steelblue")) + theme_classic()+
  theme(axis.text.x = element_text(angle=0, margin = margin(t=5, r=50)))+
  scale_x_date(date_labels="%b",date_breaks  ="1 month")+
  labs(title="Daily temperature minima", y="Temperature (?C)", x="")+
  theme(text=element_text(size=14,  family="serif"))

daymean_min_plot + theme(legend.position = "none")



####4) Analysis####

####*site: mean sd####

#OT
OT.meansd<-
  OT_TEMP%>%
  summarise(mean.ot = mean(TEMP), sd.ot = sd(TEMP))

OT.meansd

#WT
WT.meansd<-
  WT_TEMP%>%
  summarise(mean.wt = mean(TEMP), sd.wt = sd(TEMP))

WT.meansd


####*daily mean mixed model####

meanday.mm <- 
  lmer(TEMP_mean~SITE*season + (1|MONTHID), data=day_sum)

summary(meanday.mm)


#check assumptions
plot(fitted(meanday.mm),residuals(meanday.mm))
hist(residuals(meanday.mm ))
qqnorm(residuals(meanday.mm ))
vif(meanday.mm)


#ACF 
acf(residuals(meanday.mm))


#posthoc
emmeans(meanday.mm, pairwise~SITE*season)


####*daily mean linear model####
#just to take a look at ACF if month is not used as a random effect

my_lm <- 
  lm(TEMP_mean~SITE*season, data=day_sum)

summary(my_lm)


#check assumptions
plot(fitted(my_lm),residuals(my_lm))
hist(residuals(my_lm))
qqnorm(residuals(meanday.mm ))
vif(my_lm)

#ACF (code below shows the same thing)
acf(residuals(my_lm))



####*daily range mixed model####

dayrange.mm <- 
  lmer(day_range~SITE*season + (1|MONTHID), data=day_sum)

summary(dayrange.mm)

#check assumptions
plot(fitted(dayrange.mm),residuals(dayrange.mm))
hist(residuals(dayrange.mm))
qqnorm(residuals(dayrange.mm))
vif(dayrange.mm)

#ACF 
acf(residuals(dayrange.mm))


#posthoc
emmeans(dayrange.mm, pairwise~SITE*season)


####*daily max mixed model####

daymax.mm <- 
  lmer(TEMP_max~SITE*season + (1|MONTHID), data=day_sum)

summary(daymax.mm)

#check assumptions
plot(fitted(daymax.mm),residuals(daymax.mm))
hist(residuals(daymax.mm))
qqnorm(residuals(daymax.mm))
vif(daymax.mm)

#ACF 
acf(residuals(daymax.mm))


#posthoc
emmeans(daymax.mm, pairwise~SITE*season)


####*daily min mixed model####

daymin <- 
  lmer(TEMP_min~SITE*season + (1|MONTHID), data=day_sum)

summary(daymin)


#check assumptions
plot(fitted(daymin),residuals(daymin))
hist(residuals(daymin))
qqnorm(residuals(daymin))
vif(daymin)

#ACF 
acf(residuals(daymin))


#posthoc
emmeans(daymin, pairwise~SITE*season)


####___________####

####<NUTRIENTS>#####

####1) Libraries####

library(tidyverse)
library(car)

####2) Data####

NUT_data <- read_csv("NUT.csv")

#class check

NUT_data$Date <- as.Date(NUT_data$Date)
NUT_data$Site <- as.factor(NUT_data$Site)

summary(NUT_data)
head(NUT_data)

#WT subset

NUT_WT <-
  NUT_data%>%
  filter(Site =='WT')


#OT subset

NUT_OT <-
  NUT_data%>%
  filter(Site =='OT')


####3) Plots####

boxplot(BOD5~Site, data=NUT_data)
boxplot(NO3N~Site, data=NUT_data)
boxplot(NO2N~Site, data=NUT_data)
boxplot(NH3N~Site, data=NUT_data)
boxplot(PO4P~Site, data=NUT_data)


####4) Analysis####

####*B0D5####

#test for normality
hist(NUT_OT$BOD5)
shapiro.test(NUT_OT$BOD5)


hist(NUT_WT$BOD5)
shapiro.test(NUT_WT$BOD5)


#test for equal variance
leveneTest(BOD5~Site, data=NUT_data)

#test for difference between sites
wilcox.test(BOD5~Site, data=NUT_data)



####*NO2####


#test for normality
hist(NUT_OT$NO2N)
shapiro.test(NUT_OT$NO2N)


hist(NUT_WT$NO2N)
shapiro.test(NUT_WT$NO2N)

#test for equal variance
leveneTest(NO2N~Site, data=NUT_data)

#test for difference between sites
wilcox.test(NO2N~Site, data=NUT_data)



####*NO3####


#test for normality
hist(NUT_OT$NO3N)
shapiro.test(NUT_OT$NO3N)


hist(NUT_WT$NO3N)
shapiro.test(NUT_WT$NO3N)

#test for equal variance
leveneTest(NO3N~Site, data=NUT_data)

#test for difference between sites
t.test(NO3N~Site, data=NUT_data, var.equal=TRUE)



####*NH3####


#test for normality
hist(NUT_OT$NH3N)
shapiro.test(NUT_OT$NH3N)


hist(NUT_WT$NH3N)
shapiro.test(NUT_WT$NH3N)

#test for equal variance
leveneTest(NH3N~Site, data=NUT_data)


#test for difference between sites
wilcox.test(NH3N~Site, data=NUT_data)


####*PO4####


#test for normality
hist(NUT_OT$PO4P)
shapiro.test(NUT_OT$PO4P)


hist(NUT_WT$PO4P)
shapiro.test(NUT_WT$PO4P)

#test for equal variance
leveneTest(PO4P~Site, data=NUT_data)

#test for difference between sites
wilcox.test(PO4P~Site, data=NUT_data)


#Mean and sd
NUT_summary <-
  NUT_data%>%
  group_by(Site)%>%
  summarise(BOD5.mean = mean(BOD5), BOD5.sd = sd(BOD5), 
            NO2N.mean = mean(NO2N), NO2N.sd = sd(NO2N), 
            NO3N.mean = mean(NO3N), NO3N.sd = sd(NO3N),
            NH3N.mean = mean(NH3N), NH3N.sd = sd(NH3N),
            PO4P.mean = mean(PO4P), PO4P.sd = sd(PO4P))

NUT_summary


####___________####

####<FV/FM>####

####1) Libraries####

library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)

####2) Data####

#All species and sites
FVFM_ALL <- read_csv("FVFM.ALL.csv")

#PA subset
FVFM_PA <-
  FVFM_ALL%>%
  filter(SPECIES =='PA')

#AN subset
FVFM_AN <-
  FVFM_ALL%>%
  filter(SPECIES =='AN')

#PL subset
FVFM_PL <-
  FVFM_ALL%>%
  filter(SPECIES =='PL')


#all class check

#FVFM_ALL
FVFM_ALL$COL.ID <- as.factor (FVFM_ALL$COL.ID)
FVFM_ALL$SEASON <- as.factor (FVFM_ALL$SEASON)
FVFM_ALL$SITE <- as.factor (FVFM_ALL$SITE) 
FVFM_ALL$SUBSET <- as.factor (FVFM_ALL$SUBSET)
FVFM_ALL$FVFM<- as.numeric (FVFM_ALL$FVFM)
FVFM_ALL$SPECIES <-as.factor(FVFM_ALL$SPECIES)

summary(FVFM_ALL)
str(FVFM_ALL)
head(FVFM_ALL)


#subset class checks

#FVFM_PA
FVFM_PA$COL.ID <- as.factor (FVFM_PA$COL.ID)
FVFM_PA$SEASON <- as.factor (FVFM_PA$SEASON)
FVFM_PA$SITE <- as.factor (FVFM_PA$SITE) 
FVFM_PA$SUBSET <- as.factor (FVFM_PA$SUBSET)
FVFM_PA$FVFM<- as.numeric (FVFM_PA$FVFM)
FVFM_PA$SPECIES <-as.factor(FVFM_PA$SPECIES)

summary(FVFM_PA)
str(FVFM_PA)
head(FVFM_PA)


#FVFM_AN
FVFM_AN$COL.ID <- as.factor (FVFM_AN$COL.ID)
FVFM_AN$SEASON <- as.factor (FVFM_AN$SEASON)
FVFM_AN$SITE <- as.factor (FVFM_AN$SITE) 
FVFM_AN$SUBSET <- as.factor (FVFM_AN$SUBSET)
FVFM_AN$FVFM<- as.numeric (FVFM_AN$FVFM)
FVFM_AN$SPECIES <-as.factor(FVFM_AN$SPECIES)

summary(FVFM_AN)
str(FVFM_AN)
head(FVFM_AN)


#FVFM_PL
FVFM_PL$COL.ID <- as.factor (FVFM_PL$COL.ID)
FVFM_PL$SEASON <- as.factor (FVFM_PL$SEASON)
FVFM_PL$SITE <- as.factor (FVFM_PL$SITE) 
FVFM_PL$SUBSET <- as.factor (FVFM_PL$SUBSET)
FVFM_PL$FVFM<- as.numeric (FVFM_PL$FVFM)
FVFM_PL$SPECIES <-as.factor(FVFM_PL$SPECIES)

summary(FVFM_PL)
str(FVFM_PL)
head(FVFM_PL)



#Species subsets (seasonal mean, se, sd)

#PA
PA_FVFM_sum <-
  FVFM_PA%>%
  filter(SPECIES =="PA")%>%
  group_by(SEASON, SITE)%>%
  summarise(mean = mean(FVFM), sd = sd(FVFM), n = n(), se = sd / sqrt(n))

#AN
AN_FVFM_sum <-
  FVFM_AN%>%
  filter(SPECIES =="AN")%>%
  group_by(SEASON, SITE)%>%
  summarise(mean = mean(FVFM), sd = sd(FVFM), n = n(), se = sd / sqrt(n))

#PL
PL_FVFM_sum <-
  FVFM_PL%>%
  filter(SPECIES =="PL")%>%
  group_by(SEASON, SITE)%>%
  summarise(mean = mean(FVFM), sd = sd(FVFM), n = n(), se = sd / sqrt(n))




####3) Plot####

site_comparison_plot <- 
  ggplot(data=FVFM_ALL, aes(x=SEASON, y=FVFM, fill=SPECIES)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_point(aes(shape=SUBSET, group = SPECIES), 
             position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.7), 
             size=1.5) +
  scale_shape_manual(values = c(1, 16)) +
  scale_y_continuous(limits=c(0.4, 0.85), breaks=c(.4, .45,.5,.55, .6,.65, .7,.75, .8, .85))+
  scale_fill_manual(breaks = c("AN", "PA", "PL"), 
                    values=c("darkorange", "darkgreen", "darkorchid"))+
  labs(y = "Maximum quantum yeild (Fv/Fm)", x="Season")+
  theme(text=element_text(size=14,  family="serif"))+
  facet_wrap(~SITE)+
  theme_classic()

site_comparison_plot + theme(legend.position = "none")

#PA plot
PA_FVFM_plot <- 
  ggplot(PA_FVFM_sum, aes(x=SEASON, y=mean))+
  geom_line(aes(group = 1), size =1)+
  geom_point()+
  geom_point(size=1)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size =1, width=.2,
                position=position_dodge(0.05))+
  labs(y="Maximum quantum yield (Fv/Fm)", title = "PA")+
  theme_bw()+
  ylim(0.4, 0.85)+
  facet_wrap(~SITE)

PA_FVFM_plot


#AN plot
AN_FVFM_plot <- 
  ggplot(AN_FVFM_sum, aes(x=SEASON, y=mean))+
  geom_line(aes(group = 1), size =1)+
  geom_point()+
  geom_point(size=1)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size =1, width=.2,
                position=position_dodge(0.05))+
  labs(y="Maximum quantum yield (Fv/Fm)", title = "AN")+
  theme_bw()+
  ylim(0.4, 0.85)+
  facet_wrap(~SITE)

AN_FVFM_plot

#PL plot
PL_FVFM_plot <- 
  ggplot(PL_FVFM_sum, aes(x=SEASON, y=mean))+
  geom_line(aes(group = 1), size =1)+
  geom_point()+
  geom_point(size=1)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size =1, width=.2,
                position=position_dodge(0.05))+
  labs(y="Maximum quantum yield (Fv/Fm)", title = "PL")+
  theme_bw()+
  ylim(0.4, 0.85)+
  facet_wrap(~SITE)

PL_FVFM_plot




####4) Analysis####

#Subset in model
ALL.FVFM.subsetcheck <- lmer(FVFM ~ SUBSET * SPECIES *SITE *SEASON + (1|COL.ID), data=FVFM_ALL)
summary(ALL.FVFM.subsetcheck)

#subset effect is p = 0.34


#mixed model
ALL.FVFMint <- lmer(FVFM ~ SITE * SEASON * SPECIES + (1|COL.ID), data=FVFM_ALL)
summary(ALL.FVFMint)

#check assumptions
plot(fitted(ALL.FVFMint),residuals(ALL.FVFMint))
hist(residuals(ALL.FVFMint))
qqnorm(residuals(ALL.FVFMint))
vif(ALL.FVFMint)

#posthoc
FVFM_posthoc <- emmeans(ALL.FVFMint, pairwise~SITE * SEASON * SPECIES)
options(max.print = 9999)
FVFM_posthoc


####___________####

####<TOTAL LIPIDS>####

####1) Libraries####

library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)

####2) Data####

ALL_TOTAL <- read_csv("ALL_TOTAL.csv")


#class check
ALL_TOTAL$COL.ID <- as.factor(ALL_TOTAL$COL.ID)
ALL_TOTAL$SEASON <- as.factor(ALL_TOTAL$SEASON)
ALL_TOTAL$SITE <- as.factor(ALL_TOTAL$SITE)
ALL_TOTAL$SPECIES <- as.factor(ALL_TOTAL$SPECIES)

summary(ALL_TOTAL)
str(ALL_TOTAL)
head(ALL_TOTAL)


#Species subsets (seasonal mean, se, sd)

#PA
PA_TOTAL <-
  ALL_TOTAL%>%
  filter(SPECIES =="PA")%>%
  group_by(SEASON, SITE)%>%
  summarise(mean = mean(TOTAL), sd = sd(TOTAL), n = n(), se = sd / sqrt(n))

#AN
AN_TOTAL <-
  ALL_TOTAL%>%
  filter(SPECIES =="AN")%>%
  group_by(SEASON, SITE)%>%
  summarise(mean = mean(TOTAL), sd = sd(TOTAL), n = n(), se = sd / sqrt(n))

#PL
PL_TOTAL <-
  ALL_TOTAL%>%
  filter(SPECIES =="PL")%>%
  group_by(SEASON, SITE)%>%
  summarise(mean = mean(TOTAL), sd = sd(TOTAL), n = n(), se = sd / sqrt(n))


####3) Plots####

####*PA plot#### 
#(seasonal mean and se)
PA_plot <- 
  ggplot(PA_TOTAL, aes(x=SEASON, y=mean))+
  geom_line(aes(group = 1), size =1)+
  geom_point()+
  geom_point(size=1)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size =1, width=.2,
                position=position_dodge(0.05))+
  labs(y="Concentration (ng/ul)", title = "PA")+
  theme_bw()+
  ylim(0, 700)+
  facet_wrap(~SITE)

PA_plot


####*AN plot####
#(seasonal mean and se)
AN_plot <- 
  ggplot(AN_TOTAL, aes(x=SEASON, y=mean))+
  geom_line(aes(group = 1), size =1)+
  geom_point(size=1)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size =1, width=.2,
                position=position_dodge(0.05))+
  labs(y="Concentration (ng/ul)", title = "AN")+
  theme_bw()+
  ylim(0, 700)+
  facet_wrap(~SITE)

AN_plot


####*PL plot####
#(seasonal mean and se)
PL_plot <- 
  ggplot(PL_TOTAL, aes(x=SEASON, y=mean))+
  geom_line(aes(group = 1), size =1)+
  geom_point(size=1)+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size =1, width=.2,
                position=position_dodge(0.05))+
  labs(y="Concentration (ng/ul)", title = "PL")+
  ylim(0, 700)+
  theme_bw()+
  facet_wrap(~SITE)

PL_plot



####4) Analysis####

#total lipid: linear mixed-effects model
total_mm <- lmer(log(TOTAL) ~ SPECIES*SITE*SEASON + (1|COL.ID), data=ALL_TOTAL)
summary(total_mm)



#check assumptions
plot(fitted(total_mm),residuals(total_mm))
hist(residuals(total_mm))
qqnorm(residuals(total_mm))
vif(total_mm)


#posthoc
total_posthoc <- emmeans(total_mm, pairwise~SPECIES*SITE * SEASON)
options(max.print = 9999)
total_posthoc


####___________####
####<STOR:STRC LIPIDS>####


####1) Libraries####

library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(reshape2)
library(car)

####2) Data####

#for plotting individual lipid class proportions
lipid_prop <- read_csv("lipid_prop.csv")

#for storage:structural lipid ratio analysis
stor_stru <- read_csv("STOR_STRC_ratio.csv")


####*plot data: class check####

lipid_prop$COL.ID <- as.factor(lipid_prop$COL.ID)
lipid_prop$SEASON <- as.factor(lipid_prop$SEASON)
lipid_prop$SITE <- as.factor(lipid_prop$SITE)
lipid_prop$SPECIES <- as.factor(lipid_prop$SPECIES)

summary(lipid_prop)
str(lipid_prop)
head(lipid_prop)


#change wide to long data frame
longorder <- melt(lipid_prop, id.vars = c("COL.ID", "SEASON", "SITE", "SPECIES"))
head(longorder)


#First create subsets for each species

#PA
PA_PROP <-
  longorder%>%
  filter(SPECIES == "PA")

#AN
AN_PROP <-
  longorder%>%
  filter(SPECIES == "AN")

#PL
PL_PROP <-
  longorder%>%
  filter(SPECIES == "PL")



#Calculate mean, sd for each season

PA_PROP_meansd <-
  PA_PROP%>%
  group_by(SEASON, SITE, variable)%>%
  summarise(mean = mean(value), sd = sd(value))

AN_PROP_meansd <-
  AN_PROP%>%
  group_by(SEASON, SITE, variable)%>%
  summarise(mean = mean(value), sd = sd(value))

PL_PROP_meansd <-
  PL_PROP%>%
  group_by(SEASON, SITE, variable)%>%
  summarise(mean = mean(value), sd = sd(value))



####*analysis data: class check####

stor_stru$COL.ID <- as.factor(stor_stru$COL.ID)
stor_stru$SEASON <- as.factor(stor_stru$SEASON)
stor_stru$SITE <- as.factor(stor_stru$SITE)
stor_stru$SPECIES <- as.factor(stor_stru$SPECIES)

summary(stor_stru)
str(stor_stru)
head(stor_stru)



####3) Plots####

#PA
PA_stack <- ggplot(PA_PROP_meansd, aes(y=mean, x=SEASON, colour=variable, fill=variable)) + 
  geom_bar(position="fill", stat="identity")+
  labs(title="PA")+
  theme_classic()+
  facet_wrap(~SITE)

PA_stack 

col_list<-c("#8C510A", "#BF812D" ,"#DFC27D" ,"tan1" ,"#80CDC1" ,"#35978F" ,"#01665E", "#01332f") 

PA_stack + 
  scale_colour_manual(values = col_list) +
  scale_fill_manual(values = col_list) +  theme(text=element_text(size=14,  family="serif"))


#AN
AN_stack <- ggplot(AN_PROP_meansd, aes(y=mean, x=SEASON, fill=variable)) + 
  geom_bar(position="fill", stat="identity")+
  labs(title="AN")+
  theme_classic()+
  facet_wrap(~SITE)

AN_stack

col_list<-c("#8C510A", "#BF812D" ,"#DFC27D" ,"tan1" ,"#80CDC1" ,"#35978F" ,"#01665E", "#01332f") 

AN_stack + 
  scale_colour_manual(values = col_list) +
  scale_fill_manual(values = col_list) +  theme(text=element_text(size=14,  family="serif"))


#PL
PL_stack <- ggplot(PL_PROP_meansd, aes(y=mean, x=SEASON, fill=variable)) + 
  geom_bar(position="fill", stat="identity")+
  labs(title="PL")+
  theme_classic()+
  facet_wrap(~SITE)

PL_stack

col_list<-c("#8C510A", "#BF812D" ,"#DFC27D" ,"tan1" ,"#80CDC1" ,"#35978F" ,"#01665E", "#01332f")

PL_stack + 
  scale_colour_manual(values = col_list) +
  scale_fill_manual(values = col_list)  +  theme(text=element_text(size=14,  family="serif"))



####4) Analysis####

#mixed model

SSratio <- lmer(log(STOSTR) ~ SITE * SPECIES * SEASON + (1|COL.ID), data = stor_stru)
summary(SSratio)


#check assumptions
plot(fitted(SSratio),residuals(SSratio))
hist(residuals(SSratio))
qqnorm(residuals(SSratio))
vif(SSratio)

SSratio_posthoc <- emmeans(SSratio, pairwise ~ SITE * SPECIES * SEASON)
options(max.print = 9999)
SSratio_posthoc


####___________####
####< INDIV. LIPID CLASS>####

####1) Libraries####

library(tidyverse)
library(car)

####2) Data####

lipids <- read_csv("lipid_class.csv")


#class check
head(lipids)
lipids$COL.ID <- as.factor(lipids$COL.ID)
lipids$SEASON <- as.factor(lipids$SEASON)
lipids$SITE <- as.factor(lipids$SITE)
lipids$SPECIES <- as.factor(lipids$SPECIES)

summary(lipids)
str(lipids)


####*Subsets####

#sites

OT_lipids <-
  lipids %>%
  filter(SITE == "OT")

WT_lipids <-
  lipids %>%
  filter(SITE == "WT")


#species

PA_lipids <-
  lipids %>%
  filter(SPECIES == "PA")

AN_lipids <-
  lipids %>%
  filter(SPECIES == "AN")

PL_lipids <-
  lipids %>%
  filter(SPECIES == "PL")

####3) Plots####


#####*SE####

lipids_SE<-
  ggplot(data=lipids, aes(x=SEASON, y=SE, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(colour=SPECIES, group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 5), breaks=c(0,1, 2,3,4, 5))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="Sterol esters")+
  theme(text=element_text(size=14,  family="serif"))

lipids_SE

col_list<-c("darkorange", "darkgreen", "darkorchid") 

lipids_SE +
  scale_color_manual(values = col_list)+
  theme(legend.position = "none")



####***Facet by species####
PA_lipids_SE<-
  ggplot(data = PA_lipids, aes(x=SEASON, y=SE, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 5), breaks=c(0,1, 2,3,4, 5))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="P.acuta: sterol esters")+
  theme(text=element_text(size=14,  family="serif"))


PA_lipids_SE 

PA_lipids_SE + facet_wrap(~SITE) +theme_bw()


AN_lipids_SE<-
  ggplot(data = AN_lipids, aes(x=SEASON, y=SE, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 5), breaks=c(0,1, 2,3,4, 5))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="A.nana: sterol esters")+
  theme(text=element_text(size=14,  family="serif"))


AN_lipids_SE 

AN_lipids_SE + facet_wrap(~SITE) +theme_bw()




PL_lipids_SE<-
  ggplot(data = PL_lipids, aes(x=SEASON, y=SE, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 5), breaks=c(0,1, 2,3,4, 5))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="P.lutea: sterol esters")+
  theme(text=element_text(size=14,  family="serif"))


PL_lipids_SE 

PL_lipids_SE + facet_wrap(~SITE) +theme_bw()




####*WE####

lipids_WE<-
  ggplot(data=lipids, aes(x=SEASON, y=WE, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(colour=SPECIES, group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 350), breaks=c(0,50, 100, 150, 200, 250, 300, 350))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="Wax esters")+
  theme(text=element_text(size=14,  family="serif"))

lipids_WE

col_list<-c("darkorange", "darkgreen", "darkorchid") 

lipids_WE +
  scale_color_manual(values = col_list)+
  theme(legend.position = "none")






####***Facet by species####

PA_lipids_WE<-
  ggplot(data = PA_lipids, aes(x=SEASON, y=WE, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 350), breaks=c(0,50, 100, 150, 200, 250, 300, 350))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="P.acuta: wax esters")+
  theme(text=element_text(size=14,  family="serif"))


PA_lipids_WE 

PA_lipids_WE + facet_wrap(~SITE) +theme_bw()




AN_lipids_WE<-
  ggplot(data = AN_lipids, aes(x=SEASON, y=WE, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 350), breaks=c(0,50, 100, 150, 200, 250, 300, 350))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="A.nana: wax esters")+
  theme(text=element_text(size=14,  family="serif"))


AN_lipids_WE 

AN_lipids_WE + facet_wrap(~SITE) +theme_bw()




PL_lipids_WE<-
  ggplot(data = PL_lipids, aes(x=SEASON, y=WE, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 350), breaks=c(0,50, 100, 150, 200, 250, 300, 350))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="P.lutea: wax esters")+
  theme(text=element_text(size=14,  family="serif"))


PL_lipids_WE 

PL_lipids_WE + facet_wrap(~SITE) +theme_bw()


####***Analysis####

library(lme4)
library(lmerTest)
library(emmeans)


#PA
plot(PA_lipids$WE)
hist(PA_lipids$WE)

PA_WE_season.mm <- lmer(log(WE) ~ SEASON*SITE + (1|COL.ID), data = PA_lipids)
summary(PA_WE_season.mm)


#check assumptions
plot(fitted(PA_WE_season.mm),residuals(PA_WE_season.mm))
hist(residuals(PA_WE_season.mm))
qqnorm(residuals(PA_WE_season.mm))


PA_WE_season.mm_posthoc <- emmeans(PA_WE_season.mm, pairwise ~ SEASON | SITE)
PA_WE_season.mm_posthoc



#AN
plot(AN_lipids$WE)
hist(AN_lipids$WE)

AN_WE_season.mm <- lmer(WE ~ SEASON*SITE + (1|COL.ID), data = AN_lipids)
summary(AN_WE_season.mm)


#check assumptions
plot(fitted(AN_WE_season.mm),residuals(AN_WE_season.mm))
hist(residuals(AN_WE_season.mm))
qqnorm(residuals(AN_WE_season.mm))


AN_WE_season.mm_posthoc <- emmeans(AN_WE_season.mm, pairwise ~ SEASON | SITE)
AN_WE_season.mm_posthoc



#PL
plot(PL_lipids$WE)
hist(PL_lipids$WE)

PL_WE_season.mm <- lmer(WE ~ SEASON*SITE + (1|COL.ID), data = PL_lipids)
summary(PL_WE_season.mm)


#check assumptions
plot(fitted(PL_WE_season.mm),residuals(PL_WE_season.mm))
hist(residuals(PL_WE_season.mm))
qqnorm(residuals(PL_WE_season.mm))


PL_WE_season.mm_posthoc <- emmeans(PL_WE_season.mm, pairwise ~ SEASON | SITE)
PL_WE_season.mm_posthoc






####*TAG####

lipids_TAG<-
  ggplot(data=lipids, aes(x=SEASON, y=TAG, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(colour=SPECIES, group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 200), breaks=c(0,50, 100, 150, 200))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="Triacylglycerols")+
  theme(text=element_text(size=14,  family="serif"))

lipids_TAG

col_list<-c("darkorange", "darkgreen", "darkorchid") 

lipids_TAG +
  scale_color_manual(values = col_list)+
  theme(legend.position = "none")



####***Facet by species####

#PA
PA_lipids_TAG<-
  ggplot(data = PA_lipids, aes(x=SEASON, y=TAG, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 200), breaks=c(0,50, 100, 150, 200))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="P.acuta: TAG")+
  theme(text=element_text(size=14,  family="serif"))


PA_lipids_TAG 

PA_lipids_TAG + facet_wrap(~SITE) +theme_bw()




#AN
AN_lipids_TAG<-
  ggplot(data = AN_lipids, aes(x=SEASON, y=TAG, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 200), breaks=c(0,50, 100, 150, 200))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="A.nana: TAG")+
  theme(text=element_text(size=14,  family="serif"))


AN_lipids_TAG

AN_lipids_TAG + facet_wrap(~SITE) +theme_bw()



#PL
PL_lipids_TAG<-
  ggplot(data = PL_lipids, aes(x=SEASON, y=TAG, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 200), breaks=c(0,50, 100, 150, 200))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="P.lutea: TAG")+
  theme(text=element_text(size=14,  family="serif"))


PL_lipids_TAG 

PL_lipids_TAG + facet_wrap(~SITE) +theme_bw()







####*FFA####

lipids_FFA<-
  ggplot(data=lipids, aes(x=SEASON, y=FFA, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(colour=SPECIES, group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 350), breaks=c(0,50, 100, 150, 200, 250, 300, 350))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="Free fatty acids")+
  theme(text=element_text(size=14,  family="serif"))

lipids_FFA

col_list<-c("darkorange", "darkgreen", "darkorchid") 

lipids_FFA +
  scale_color_manual(values = col_list)+
  theme(legend.position = "none")



####***Facet by species####

PA_lipids_FFA<-
  ggplot(data = PA_lipids, aes(x=SEASON, y=FFA, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 350), breaks=c(0,50, 100, 150, 200, 250, 300, 350))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="P.acuta: free fatty acids")+
  theme(text=element_text(size=14,  family="serif"))


PA_lipids_FFA 

PA_lipids_FFA + facet_wrap(~SITE) +theme_bw()




AN_lipids_FFA<-
  ggplot(data = AN_lipids, aes(x=SEASON, y=FFA, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 350), breaks=c(0,50, 100, 150, 200, 250, 300, 350))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="A.nana: free fatty acids")+
  theme(text=element_text(size=14,  family="serif"))


AN_lipids_FFA 

AN_lipids_FFA + facet_wrap(~SITE) +theme_bw()





PL_lipids_FFA<-
  ggplot(data = PL_lipids, aes(x=SEASON, y=FFA, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 350), breaks=c(0,50, 100, 150, 200, 250, 300, 350))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="P.lutea: free fatty acids")+
  theme(text=element_text(size=14,  family="serif"))


PL_lipids_FFA 

PL_lipids_FFA + facet_wrap(~SITE) +theme_bw()


####***Analysis####

library(lme4)
library(lmerTest)
library(emmeans)


#PA
plot(PA_lipids$FFA)
hist(PA_lipids$FFA)

PA_FFA_season.mm <- lmer(FFA ~ SEASON*SITE + (1|COL.ID), data = PA_lipids)
summary(PA_FFA_season.mm)


#check assumptions
plot(fitted(PA_FFA_season.mm),residuals(PA_FFA_season.mm))
hist(residuals(PA_FFA_season.mm))
qqnorm(residuals(PA_FFA_season.mm))


PA_FFA_season.mm_posthoc <- emmeans(PA_FFA_season.mm, pairwise ~ SEASON | SITE)
PA_FFA_season.mm_posthoc




#AN
plot(AN_lipids$FFA)
hist(AN_lipids$FFA)

AN_FFA_season.mm <- lmer(FFA ~ SEASON*SITE + (1|COL.ID), data = AN_lipids)
summary(AN_FFA_season.mm)


#check assumptions
plot(fitted(AN_FFA_season.mm),residuals(AN_FFA_season.mm))
hist(residuals(AN_FFA_season.mm))
qqnorm(residuals(AN_FFA_season.mm))


AN_FFA_season.mm_posthoc <- emmeans(AN_FFA_season.mm, pairwise ~ SEASON | SITE)
AN_FFA_season.mm_posthoc



#PL
plot(PL_lipids$FFA)
hist(PL_lipids$FFA)

PL_FFA_season.mm <- lmer(FFA ~ SEASON*SITE + (1|COL.ID), data = PL_lipids)
summary(PL_FFA_season.mm)


#check assumptions
plot(fitted(PL_FFA_season.mm),residuals(PL_FFA_season.mm))
hist(residuals(PL_FFA_season.mm))
qqnorm(residuals(PL_FFA_season.mm))


PL_FFA_season.mm_posthoc <- emmeans(PL_FFA_season.mm, pairwise ~ SEASON | SITE)
PL_FFA_season.mm_posthoc







####*Col####

lipids_Col<-
  ggplot(data=lipids, aes(x=SEASON, y=Col, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(colour=SPECIES, group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 100), breaks=c(0,20, 40, 60, 80, 100))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="Cholesterol")+
  theme(text=element_text(size=14,  family="serif"))

lipids_Col

col_list<-c("darkorange", "darkgreen", "darkorchid") 

lipids_Col +
  scale_color_manual(values = col_list)+
  theme(legend.position = "none")



####***Facet by species####

#PA
PA_lipids_Col<-
  ggplot(data = PA_lipids, aes(x=SEASON, y=Col, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 100), breaks=c(0,20, 40, 60, 80, 100))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="P.acuta: Col")+
  theme(text=element_text(size=14,  family="serif"))


PA_lipids_Col 

PA_lipids_Col + facet_wrap(~SITE) +theme_bw()




#AN
AN_lipids_Col<-
  ggplot(data = AN_lipids, aes(x=SEASON, y=Col, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 100), breaks=c(0,20, 40, 60, 80, 100))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="A.nana: Col")+
  theme(text=element_text(size=14,  family="serif"))


AN_lipids_Col

AN_lipids_Col + facet_wrap(~SITE) +theme_bw()



#PL
PL_lipids_Col<-
  ggplot(data = PL_lipids, aes(x=SEASON, y=Col, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 100), breaks=c(0,20, 40, 60, 80, 100))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="P.lutea: Col")+
  theme(text=element_text(size=14,  family="serif"))


PL_lipids_Col 

PL_lipids_Col + facet_wrap(~SITE) +theme_bw()




####*PE####

lipids_PE<-
  ggplot(data=lipids, aes(x=SEASON, y=PE, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(colour=SPECIES, group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 70), breaks=c(0,10, 20, 30, 40, 50, 60, 70))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="Phosphatidylethanolamine")+
  theme(text=element_text(size=14,  family="serif"))

lipids_PE

col_list<-c("darkorange", "darkgreen", "darkorchid") 

lipids_PE +
  scale_color_manual(values = col_list)+
  theme(legend.position = "none")



####***Facet by species####

#PA
PA_lipids_PE<-
  ggplot(data = PA_lipids, aes(x=SEASON, y=PE, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 100), breaks=c(0,20, 40, 60, 80, 100))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="P.acuta: PE")+
  theme(text=element_text(size=14,  family="serif"))


PA_lipids_PE 

PA_lipids_PE + facet_wrap(~SITE) +theme_bw()




#AN
AN_lipids_PE<-
  ggplot(data = AN_lipids, aes(x=SEASON, y=PE, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 100), breaks=c(0,20, 40, 60, 80, 100))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="A.nana: PE")+
  theme(text=element_text(size=14,  family="serif"))


AN_lipids_PE

AN_lipids_PE + facet_wrap(~SITE) +theme_bw()



#PL
PL_lipids_PE<-
  ggplot(data = PL_lipids, aes(x=SEASON, y=PE, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 100), breaks=c(0,20, 40, 60, 80, 100))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="P.lutea: PE")+
  theme(text=element_text(size=14,  family="serif"))


PL_lipids_PE 

PL_lipids_PE + facet_wrap(~SITE) +theme_bw()





####*PC####

lipids_PC<-
  ggplot(data=lipids, aes(x=SEASON, y=PC, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(colour=SPECIES, group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 100), breaks=c(0,20,40,60,80,100))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="Phosphatidylcholine")+
  theme(text=element_text(size=14,  family="serif"))

lipids_PC

col_list<-c("darkorange", "darkgreen", "darkorchid") 

lipids_PC +
  scale_color_manual(values = col_list)+
  theme(legend.position = "none")


####***Facet by species####

#PA
PA_lipids_PC<-
  ggplot(data = PA_lipids, aes(x=SEASON, y=PC, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 100), breaks=c(0,20,40,60,80,100))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="P.acuta: PC")+
  theme(text=element_text(size=14,  family="serif"))


PA_lipids_PC 

PA_lipids_PC + facet_wrap(~SITE) +theme_bw()




#AN
AN_lipids_PC<-
  ggplot(data = AN_lipids, aes(x=SEASON, y=PC, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 100), breaks=c(0,20,40,60,80,100))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="A.nana: PC")+
  theme(text=element_text(size=14,  family="serif"))


AN_lipids_PC 

AN_lipids_PC + facet_wrap(~SITE) +theme_bw()



#PL
PL_lipids_PC<-
  ggplot(data = PL_lipids, aes(x=SEASON, y=PC, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 100), breaks=c(0,20,40,60,80,100))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="P.lutea: PC")+
  theme(text=element_text(size=14,  family="serif"))


PL_lipids_PC 

PL_lipids_PC + facet_wrap(~SITE) +theme_bw()


####*LPC####

lipids_LPC<-
  ggplot(data=lipids, aes(x=SEASON, y=LPC, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(colour=SPECIES, group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 30), breaks=c(0,10, 20,30))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="Lyso-phosphatidylcholine")+
  theme(text=element_text(size=14,  family="serif"))

lipids_LPC

col_list<-c("darkorange", "darkgreen", "darkorchid") 

lipids_LPC +
  scale_color_manual(values = col_list)+
  theme(legend.position = "none")




####***Facet by species####

#PA
PA_lipids_LPC<-
  ggplot(data = PA_lipids, aes(x=SEASON, y=LPC, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 30), breaks=c(0,10, 20,30))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="P.acuta: LPC")+
  theme(text=element_text(size=14,  family="serif"))


PA_lipids_LPC 

PA_lipids_LPC + facet_wrap(~SITE) +theme_bw()




#AN
AN_lipids_LPC<-
  ggplot(data = AN_lipids, aes(x=SEASON, y=LPC, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 30), breaks=c(0,10, 20,30))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="A.nana: LPC")+
  theme(text=element_text(size=14,  family="serif"))


AN_lipids_LPC 

AN_lipids_LPC + facet_wrap(~SITE) +theme_bw()



#PL
PL_lipids_LPC<-
  ggplot(data = PL_lipids, aes(x=SEASON, y=LPC, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 30), breaks=c(0,10, 20,30))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="P.lutea: LPC")+
  theme(text=element_text(size=14,  family="serif"))


PL_lipids_LPC 

PL_lipids_LPC + facet_wrap(~SITE) +theme_bw()



####*LPC:PC ratio####

#create new column with LPC:PC ratio
ratio_check <-
  lipids%>%
  mutate(LPC_PC_ratio = LPC/PC)

summary(ratio_check)


#species subsets

#PA
PA_lipids_rc <-
  ratio_check %>%
  filter(SPECIES == "PA")

summary(PA_lipids_rc)
#removed one row with Inf value
PA_lipids_rc <-  PA_lipids_rc[-21,]
summary(PA_lipids_rc)

#AN
AN_lipids_rc <-
  ratio_check %>%
  filter(SPECIES == "AN")

summary(AN_lipids_rc)

#PL
PL_lipids_rc <-
  ratio_check %>%
  filter(SPECIES == "PL")

summary(PL_lipids_rc)




#plot LPC:PC ratio
lipids_LPC_PC<-
  ggplot(data=ratio_check, aes(x=SEASON, y=LPC_PC_ratio, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(colour=SPECIES, group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0,20), breaks=c(0,10, 20))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="LPC:PC ratio")+
  theme(text=element_text(size=14,  family="serif"))

lipids_LPC_PC

col_list<-c("darkorange", "darkgreen", "darkorchid") 

lipids_LPC_PC +
  scale_color_manual(values = col_list)+
  theme(legend.position = "none")



####***Facet by species####

#PA
PA_lipids_LPC_PC<-
  ggplot(data = PA_lipids_rc, aes(x=SEASON, y=LPC_PC_ratio, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 16.5), breaks=c(0,4,8,12,16))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="P.acuta: LPC:PC")+
  theme(text=element_text(size=14,  family="serif"))


PA_lipids_LPC_PC

PA_lipids_LPC_PC + facet_wrap(~SITE) +theme_bw()

#PA with same scale as other species (i.e., outlier outside of y axis range)
PA_lipids_LPC_PC_same_scale<-
  ggplot(data = PA_lipids_rc, aes(x=SEASON, y=LPC_PC_ratio, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 6), breaks=c(0,2,4,6))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="P.acuta: LPC:PC")+
  theme(text=element_text(size=14,  family="serif"))


PA_lipids_LPC_PC_same_scale

PA_lipids_LPC_PC_same_scale + facet_wrap(~SITE) +theme_bw()


#AN
AN_lipids_LPC_PC<-
  ggplot(data = AN_lipids_rc, aes(x=SEASON, y=LPC_PC_ratio, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 6), breaks=c(0,2,4,6))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="A.nana: LPC:PC")+
  theme(text=element_text(size=14,  family="serif"))


AN_lipids_LPC_PC 

AN_lipids_LPC_PC + facet_wrap(~SITE) +theme_bw()



#PL
PL_lipids_LPC_PC<-
  ggplot(data = PL_lipids_rc, aes(x=SEASON, y=LPC_PC_ratio, fill=SITE)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(aes(group = SITE), 
             position = position_jitterdodge(jitter.width = 0.5, dodge.width = 0.75), 
             size=1.75) +
  scale_y_continuous(limits=c(0, 6), breaks=c(2,4,6))+
  scale_fill_manual(breaks = c("OT", "WT"), 
                    values=c("snow4", "steelblue")) + theme_classic()+
  labs(y = "Concentration (ng/ul)", x="Season", title="P.lutea: LPC:PC")+
  theme(text=element_text(size=14,  family="serif"))


PL_lipids_LPC_PC 

PL_lipids_LPC_PC + facet_wrap(~SITE) +theme_bw()




####***Analysis####

library(lme4)
library(lmerTest)
library(emmeans)
library(car)

#PA
plot(PA_lipids_rc$LPC_PC_ratio)
hist(PA_lipids_rc$LPC_PC_ratio)


PA_LPC_PC_ratio_season.mm <- lmer(log(LPC_PC_ratio) ~ SEASON*SITE + (1|COL.ID), data = PA_lipids_rc)
summary(PA_LPC_PC_ratio_season.mm)


#check assumptions
plot(fitted(PA_LPC_PC_ratio_season.mm),residuals(PA_LPC_PC_ratio_season.mm))
hist(residuals(PA_LPC_PC_ratio_season.mm))
qqnorm(residuals(PA_LPC_PC_ratio_season.mm))
vif(PA_LPC_PC_ratio_season.mm)


PA_LPC_PC_ratio_season.mm_posthoc <- emmeans(PA_LPC_PC_ratio_season.mm, pairwise ~ SEASON | SITE)
PA_LPC_PC_ratio_season.mm_posthoc


#AN
plot(AN_lipids_rc$LPC_PC_ratio)
hist(AN_lipids_rc$LPC_PC_ratio)

AN_LPC_PC_ratio_season.mm <- lmer(LPC_PC_ratio ~ SEASON*SITE + (1|COL.ID), data = AN_lipids_rc)
summary(AN_LPC_PC_ratio_season.mm)


#check assumptions
plot(fitted(AN_LPC_PC_ratio_season.mm),residuals(AN_LPC_PC_ratio_season.mm))
hist(residuals(AN_LPC_PC_ratio_season.mm))
qqnorm(residuals(AN_LPC_PC_ratio_season.mm))
vif(AN_LPC_PC_ratio_season.mm)


AN_LPC_PC_ratio_season.mm_posthoc <- emmeans(AN_LPC_PC_ratio_season.mm, pairwise ~ SEASON | SITE)
AN_LPC_PC_ratio_season.mm_posthoc



#PL
plot(PL_lipids_rc$LPC_PC_ratio)
hist(PL_lipids_rc$LPC_PC_ratio)

PL_LPC_PC_ratio_season.mm <- lmer(LPC_PC_ratio ~ SEASON*SITE + (1|COL.ID), data = PL_lipids_rc)
summary(PL_LPC_PC_ratio_season.mm)


#check assumptions
plot(fitted(PL_LPC_PC_ratio_season.mm),residuals(PL_LPC_PC_ratio_season.mm))
hist(residuals(PL_LPC_PC_ratio_season.mm))
qqnorm(residuals(PL_LPC_PC_ratio_season.mm))
vif(PL_LPC_PC_ratio_season.mm)


PL_LPC_PC_ratio_season.mm_posthoc <- emmeans(PL_LPC_PC_ratio_season.mm, pairwise ~ SEASON | SITE)
PL_LPC_PC_ratio_season.mm_posthoc



####___________####
####<SYMBIODINIACEAE>####

####Libraries####

library(tidyverse)

####Data####

SYM_PA <- read_csv("PA_SYM.csv")
SYM_AN <- read_csv("AN_SYM.csv")
SYM_PL <- read_csv("PL_SYM.csv")


#Class checks

#PA
SYM_PA$SEASON <- as.factor(SYM_PA$SEASON)
SYM_PA$GENUS <- as.factor(SYM_PA$GENUS)

#AN
SYM_AN$SEASON <- as.factor(SYM_AN$SEASON)
SYM_AN$GENUS <- as.factor(SYM_AN$GENUS)


#PL
SYM_PL$SEASON <- as.factor(SYM_PL$SEASON)
SYM_PL$GENUS <- as.factor(SYM_PL$GENUS)

####Plots####

####*PA plot####

PA.sym.plot <-
  ggplot(SYM_PA, aes(y=PROP, x=SEASON, fill=GENUS)) + 
  geom_bar(position="fill", stat="identity")+
  facet_wrap(~SITE)+
  labs(y = "Proportion of colonies", x="", title ="PA")+
  theme(text=element_text(size=14,  family="serif"))+
  theme_bw()

col_list<-c("snow3", "snow4", "black") 

PA.sym.plot + 
  scale_colour_manual(values = col_list) +
  scale_fill_manual(values = col_list)+
  theme(legend.position = "none")



####*AN plot####

AN.sym.plot <-
  ggplot(SYM_AN, aes(y=PROP, x=SEASON, fill=GENUS)) + 
  geom_bar(position="fill", stat="identity")+
  facet_wrap(~SITE)+
  labs(y = "Proportion of colonies", x="", title ="AN")+
  theme(text=element_text(size=14,  family="serif"))+
  theme_bw()

col_list<-c("snow3", "snow4", "black") 

AN.sym.plot + 
  scale_colour_manual(values = col_list) +
  scale_fill_manual(values = col_list)+
  theme(legend.position = "none")


####*PL plot####

PL.sym.plot <-
  ggplot(SYM_PL, aes(y=PROP, x=SEASON, fill=GENUS)) + 
  geom_bar(position="fill", stat="identity")+
  facet_wrap(~SITE)+
  labs(y = "Proportion of colonies", x="", title ="PL")+
  theme(text=element_text(size=14,  family="serif"))+
  theme_bw()

col_list<-c("snow3", "snow4", "black") 

PL.sym.plot + 
  scale_colour_manual(values = col_list) +
  scale_fill_manual(values = col_list)+
  theme(legend.position = "none")



####_____END______####
