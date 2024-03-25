###Lonepine analysis folder###
setwd("/Users/wksmith/Rworkspace/MSI_Lone_Pine")

#Packages####################################################################
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
##Overstory Analysis##################################################################
#Read 2023
treatment2023 = read_csv("CSV2023_forR/Treatment_2023.csv")
overstory2023 = read_csv("CSV2023_forR/LonePine_OverStory_2023_23edit2.csv")
treatment2023 = treatment2023[c(4,10)] #keep only plot and treatment information
overstory2023 = overstory2023 %>% left_join(treatment2023, by = "Plot")
overstory2023 = transform(overstory2023,Height = as.numeric(Height))
#Read 2020
overstory2020 = read_csv("CSV2023_forR/LonePine_OverStory_2020_23edit2.csv")
overstory2020$DBH = overstory2020$DBH*0.393701 #convert from cm to in (2020 data only)
overstory2020$Phase = 'pre' #convert to 'pre' (2020 data only)

###
overstory2020_simple = overstory2020[c(9,12,4,5,20)]
overstory2020_simple = cbind(overstory2020_simple, Year=2020)
overstory2023_simple = overstory2023[c(4,11,28,9,14)]
overstory2023_simple = cbind(overstory2023_simple, Year=2023)
colnames(overstory2020_simple) = colnames(overstory2023_simple)
overstory_simple_merge = rbind(overstory2020_simple,overstory2023_simple)
overstory_simple_merge = subset(overstory_simple_merge, Plot!=86 & Plot!=87 & Plot!=133 & Plot!=137 & Plot!=228 & Plot!=156 & Plot!=158 & Plot!=230) #remove problematic plots

####PIPO Sumstats####
#Summarizes total ponderosa basal area by plot and export .csv files 
overstory_PIPO2023=subset(overstory2023, Species=="PIPO")
overstory_PIPO2020=subset(overstory2020, Species=="PIPO")
overstory_QUGA2023=subset(overstory2023, Species=="QUGA")
overstory_QUGA2020=subset(overstory2020, Species=="QUGA")

#What are the units on this calculation (inches)
#PIPO
PIPO2023 = overstory_PIPO2023 %>% group_by(Plot) %>% 
  summarise(n=n(),DBH_mean = mean(DBH,na.rm=T),DBH_sd = sd(DBH,na.rm=T),BA_sum = sum(DBH*DBH*0.005454*10,na.rm=T),BA_sd = sd(DBH*DBH*0.005454*10,na.rm=T),DBH_sd = sd(DBH,na.rm=T),Height_mean = mean(Height,na.rm=T),Height_sd = sd(Height,na.rm=T),year='2023')
PIPO2023 = merge(treatment2023,PIPO2023,by="Plot")

PIPO2020 = overstory_PIPO2020 %>% group_by(Plot) %>% 
  summarise(n=n(),Treatment='pre',DBH_mean = mean(DBH,na.rm=T),DBH_sd = sd(DBH,na.rm=T),BA_sum = sum(DBH*DBH*0.005454*10,na.rm=T),BA_sd = sd(DBH*DBH*0.005454*10,na.rm=T),Height_mean = mean(Height,na.rm=T),Height_sd = sd(Height,na.rm=T),year='2020')
PIPO2020 = subset(PIPO2020, !(is.na(Plot)))

PIPOmerge = rbind(PIPO2020,PIPO2023)

PIPOmerge = subset(PIPOmerge, Plot!=86 & Plot!=87 & Plot!=133 & Plot!=137 & Plot!=228 & Plot!=156 & Plot!=158 & Plot!=230) #remove problematic plots

#QUGA
QUGA2023 = overstory_QUGA2023 %>% group_by(Plot) %>% 
  summarise(n=n(),DBH_mean = mean(DBH,na.rm=T),DBH_sd = sd(DBH,na.rm=T),BA_sum = sum(DBH*DBH*0.005454*10,na.rm=T),BA_sd = sd(DBH*DBH*0.005454*10,na.rm=T),Height_mean = mean(Height,na.rm=T),Height_sd = sd(Height,na.rm=T),year='2023')
QUGA2023 = merge(treatment2023,QUGA2023,by="Plot")

QUGA2020 = overstory_QUGA2020 %>% group_by(Plot) %>% 
  summarise(n=n(),Treatment='pre',DBH_mean = mean(DBH,na.rm=T),DBH_sd = sd(DBH,na.rm=T),BA_sum = sum(DBH*DBH*0.005454*10,na.rm=T),BA_sd = sd(DBH*DBH*0.005454*10,na.rm=T),Height_mean = mean(Height,na.rm=T),Height_sd = sd(Height,na.rm=T),year='2020')
QUGA2020 = subset(QUGA2020, !(is.na(Plot)))

QUGAmerge = rbind(QUGA2020,QUGA2023)

QUGAmerge = subset(QUGAmerge, Plot!=86 & Plot!=87 & Plot!=133 & Plot!=137 & Plot!=228 & Plot!=156 & Plot!=158 & Plot!=230)


###Plot PIPO Comparison####
ggplot(PIPOmerge, aes(x = year, y = DBH_mean, fill = Treatment)) +
  geom_bar(stat="identity",position=position_dodge()) +
  facet_wrap(~Plot)+
  scale_y_continuous("DBH",limits = c(-2,50)) +
  scale_x_discrete("Year") +
  geom_text(aes(label=paste("n=",n)), vjust=-1, color="black", size=3)+
  geom_errorbar(aes(ymax = DBH_mean+DBH_sd, ymin = DBH_mean-DBH_sd), width=.2,position=position_dodge(.9)) +
  theme(text = element_text(size = 40)) +
  theme_bw()
ggsave(paste('Figures/PIPO_DBH_2020_2023_Comparison_byPlot.png',sep=''),dpi=300,width=250,height=200,units='mm')

ggplot(PIPOmerge, aes(x = year, y = BA_sum, fill = Treatment)) +
  geom_bar(stat="identity",position=position_dodge()) +
  facet_wrap(~Plot)+
  scale_y_continuous("BA",limits = c(-5,450)) +
  scale_x_discrete("Year") +
  geom_text(aes(label=paste("n=",n)), vjust=-1, color="black", size=3)+
  geom_errorbar(aes(ymax = BA_sum+BA_sd, ymin = BA_sum-BA_sd), width=.2,position=position_dodge(.9)) +
  theme(text = element_text(size = 40)) +
  theme_bw()
ggsave(paste('Figures/PIPO_BA_2020_2023_Comparison_byPlot.png',sep=''),dpi=300,width=250,height=200,units='mm')

ggplot(PIPOmerge, aes(x = year, y = Height_mean, fill = Treatment)) +
  geom_bar(stat="identity",position=position_dodge()) +
  facet_wrap(~Plot)+
  scale_y_continuous("Height",limits = c(-10,150)) +
  scale_x_discrete("Year") +
  geom_text(aes(label=paste("n=",n)), vjust=-1, color="black", size=3) +
  geom_errorbar(aes(ymax = Height_mean+Height_sd, ymin = Height_mean-Height_sd), width=.2,position=position_dodge(.9)) +
  theme(text = element_text(size = 40)) +
  theme_bw()
ggsave(paste('Figures/PIPO_Height_2020_2023_Comparison_byPlot.png',sep=''),dpi=300,width=250,height=200,units='mm')

###Plot QUGA Comparison####
ggplot(QUGAmerge, aes(x = year, y = DBH_mean, fill = Treatment)) +
  geom_bar(stat="identity",position=position_dodge()) +
  facet_wrap(~Plot)+
  scale_y_continuous("DBH",limits = c(-5,30)) +
  scale_x_discrete("Year") +
  geom_text(aes(label=paste("n=",n)), vjust=-1, color="black", size=3)+
  geom_errorbar(aes(ymax = DBH_mean+DBH_sd, ymin = DBH_mean-DBH_sd), width=.2,position=position_dodge(.9)) +
  theme(text = element_text(size = 40)) +
  theme_bw()
ggsave(paste('Figures/QUGA_DBH_2020_2023_Comparison_byPlot.png',sep=''),dpi=300,width=250,height=200,units='mm')

ggplot(QUGAmerge, aes(x = year, y = BA_sum, fill = Treatment)) +
  geom_bar(stat="identity",position=position_dodge()) +
  facet_wrap(~Plot)+
  scale_y_continuous("BA",limits = c(-5,40)) +
  scale_x_discrete("Year") +
  geom_text(aes(label=paste("n=",n)), vjust=-1, color="black", size=3)+
  geom_errorbar(aes(ymax = BA_sum+BA_sd, ymin = BA_sum-BA_sd), width=.2,position=position_dodge(.9)) +
  theme(text = element_text(size = 40)) +
  theme_bw()
ggsave(paste('Figures/QUGA_BA_2020_2023_Comparison_byPlot.png',sep=''),dpi=300,width=250,height=200,units='mm')

ggplot(QUGAmerge, aes(x = year, y = Height_mean, fill = Treatment)) +
  geom_bar(stat="identity",position=position_dodge()) +
  facet_wrap(~Plot)+
  scale_y_continuous("Height",limits = c(-1,30)) +
  scale_x_discrete("Year") +
  geom_text(aes(label=paste("n=",n)), vjust=-1, color="black", size=3)+
  geom_errorbar(aes(ymax = Height_mean+Height_sd, ymin = Height_mean-Height_sd), width=.2,position=position_dodge(.9)) +
  theme(text = element_text(size = 40)) +
  theme_bw()
ggsave(paste('Figures/QUGA_Height_2020_2023_Comparison_byPlot.png',sep=''),dpi=300,width=250,height=200,units='mm')

###############################################
#Summarizes total ponderosa basal area by plot and export .csv files 
#overstory_simple_merge_STS = subset(overstory_simple_merge, Area=="STS to LTR" | Area=="STS" | Area=="LTR")
STStoLTR_Plots=unique(overstory_simple_merge$Plot[which(overstory_simple_merge$Area=="STS to LTR")])
overstory_simple_merge_STS = subset(overstory_simple_merge, Plot >= 83 & Plot!=100 & Plot!=101 & Plot!=231 & Plot <=275) #plots that are not post-treatment in 2023

#All Species
ggplot(overstory_simple_merge_STS, aes(x = DBH, fill = Treatment)) +
  geom_histogram(alpha=0.5, color='black',binwidth=2, position="identity") +
  scale_y_continuous("Number of Trees",limits = c(0,45)) +
  scale_x_continuous("DBH") +
  theme(text = element_text(size = 40)) +
  theme_bw()
ggsave(paste('Figures/STStoLTR_2020_2023.png',sep=''),dpi=300,width=250,height=200,units='mm')

#By Species
ggplot(overstory_simple_merge_STS, aes(x = DBH, fill = Treatment)) +
  geom_histogram(alpha=0.5,color='black',binwidth=2, position="identity") +
  facet_wrap(~Species)+
  scale_y_continuous("Number of Trees") +
  scale_x_continuous("DBH") +
  theme(text = element_text(size = 40)) +
  theme_bw()
ggsave(paste('Figures/STStoLTR_2020_2023_bySpecies.png',sep=''),dpi=300,width=250,height=200,units='mm')

#PIPO by plot
overstory_simple_merge_STS_PIPO=subset(overstory_simple_merge_STS, Species=="PIPO")
ggplot(overstory_simple_merge_STS_PIPO, aes(x = DBH, fill = Treatment)) +
  geom_histogram(alpha=0.5,color='black',binwidth=2, position="identity") +
  scale_y_continuous("Number of Trees") +
  scale_x_continuous("DBH") +
  facet_wrap(~Plot)+
  theme(text = element_text(size = 40)) +
  theme_bw()
ggsave(paste('Figures/STStoLTR_2020_2023_PIPO_byPlot.png',sep=''),dpi=300,width=250,height=200,units='mm')

#QUGA by plot
overstory_simple_merge_STS_QUGA=subset(overstory_simple_merge_STS, Species=="QUGA")
ggplot(overstory_simple_merge_STS_QUGA, aes(x = DBH, fill = Treatment)) +
  geom_histogram(alpha=0.5,color='black',binwidth=2, position="identity") +
  scale_y_continuous("Number of Trees") +
  scale_x_continuous("DBH") +
  facet_wrap(~Plot)+
  theme(text = element_text(size = 40)) +
  theme_bw()
ggsave(paste('Figures/STStoLTR_2020_2023_QUGA_byPlot.png',sep=''),dpi=300,width=250,height=200,units='mm')

##########################################################################################################
#Read Salter
salter2023 = read_csv("CSV2023_forR/SALTER_OVER_2023_forGIS.csv")
salter_PIPO_2023=subset(salter2023, Species=="PIPO")
#All Species
ggplot(salter2023, aes(x = DBH, fill = Species)) +
  geom_histogram(alpha=0.5, color='black',binwidth=2, position="identity") +
  facet_wrap(~Unit)+
  scale_y_continuous("Number of Trees") +
  scale_x_continuous("DBH") +
  theme(text = element_text(size = 40)) +
  theme_bw()
ggsave(paste('Figures/Salter_DBH_2023.png',sep=''),dpi=300,width=250,height=200,units='mm')

#PIPO
ggplot(salter_PIPO_2023, aes(x = DBH)) +
  geom_histogram(alpha=0.5, color='black',binwidth=2, position="identity") +
  scale_y_continuous("Number of Trees") +
  geom_vline(xintercept = c(12,16,20),linetype = "dashed",color='gray')+
  scale_x_continuous("DBH (inch)") +
  theme(text = element_text(size = 40)) +
  theme_bw()
ggsave(paste('Figures/Salter_PIPO_DBH_2023.png',sep=''),dpi=300,width=250,height=200,units='mm')

ggplot(salter_PIPO_2023, aes(x = DBH)) +
  geom_histogram(alpha=0.5, color='black',binwidth=2, position="identity") +
  facet_wrap(~Unit)+
  scale_y_continuous("Number of Trees") +
  geom_vline(xintercept = c(12,16,20),linetype = "dashed",color='gray')+
  scale_x_continuous("DBH (inch)") +
  theme(text = element_text(size = 40)) +
  theme_bw()
ggsave(paste('Figures/Salter_PIPO_DBH_2023_byUnit.png',sep=''),dpi=300,width=250,height=200,units='mm')

#Read Salter Stats
salterTable2023 = read_csv("CSV2023_forR/SALTER_2023_OVERSTORY_TABLE.csv")
salterStat_AgeMean = salterTable2023 %>% group_by(Unit) %>% 
    summarise(Age12_mean = mean(Age_12,na.rm=T),Age16_mean = mean(Age_16,na.rm=T),Age20_mean = mean(Age_20,na.rm=T))
salterStat_AgeMean = subset(salterStat_AgeMean, !is.na(Unit))
salterStat_AgeN = salterTable2023 %>% group_by(Unit) %>% 
  summarise(Age12_n = sum(!is.na(Age_12)),Age16_n = sum(!is.na(Age_16)),Age20_n = sum(!is.na(Age_20)))
salterStat_AgeN = subset(salterStat_AgeN, !is.na(Unit))
salterStat_DBHMean = salterTable2023 %>% group_by(Unit) %>% 
  summarise(DBH12_mean = mean(DBH_12,na.rm=T),DBH16_mean = mean(DBH_16,na.rm=T),DBH20_mean = mean(DBH_20,na.rm=T))
salterStat_DBHMean = subset(salterStat_DBHMean, !is.na(Unit))

salterStat_AgeMean_long <- salterStat_AgeMean %>% pivot_longer(cols = 2:4,
                                       names_to = "Index", 
                                       values_to = "AGE")
salterStat_DBHMean_long <- salterStat_DBHMean %>% pivot_longer(cols = 2:4,
                                       names_to = "Index",
                                       values_to = "DBH")
salterStat_AgeN_long <- salterStat_AgeN %>% pivot_longer(cols = 2:4,
                                       names_to = "Index", 
                                       values_to = "N")
salterStat_long <- cbind(salterStat_AgeN_long,salterStat_AgeMean_long$AGE,salterStat_DBHMean_long$DBH)
colnames(salterStat_long) <- c('Unit','Index','N','Age','DBH')

ggplot(salterStat_long, aes(x = Index, y = Age)) +
  geom_bar(stat="identity",position=position_dodge(),fill='gray',color='black') +
  facet_wrap(~Unit)+
  geom_text(aes(label=paste("n=",N)), vjust=-1, color="black", size=3)+
  labs(y = "Age (years)", x = "DBH Class")+
  scale_y_continuous(limits = c(0,120)) +
  scale_x_discrete(labels=c('12 to 16 inch', '16 to 20 inch', '> 20 inch'),guide = guide_axis(angle = 45))+
  theme(text = element_text(size = 40)) +
  theme_bw()
ggsave(paste('Figures/Salter_PIPO_DBH_Age_Classes_2023.png',sep=''),dpi=300,width=250,height=200,units='mm')


ggplot(salterStat_long, aes(x=DBH, y=Age,color=Index)) +
  #scale_color_manual(values=c("#3288BD","#99D594","#5E4FA2","#D53E4F","#9E0142"))+
  geom_point(size=3)+
  geom_smooth(method=lm,aes(group = Index))+
  stat_cor(aes(group = Index,label = after_stat(rr.label)),geom = "label",show.legend = FALSE)+
  labs(x='DBH (inch)',y='Age (year)')+
  theme_bw()+
  #theme(legend.position = c(0.8, 0.2),legend.title = element_blank(),legend.text=element_text(size=16)) +
  theme(text = element_text(size = 20))
ggsave(paste('Figures/Salter_PIPO_DBH_Age_Regression_2023.png',sep=''),dpi=300,width=250,height=200,units='mm')


