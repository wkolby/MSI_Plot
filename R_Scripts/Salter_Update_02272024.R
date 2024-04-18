###Lonepine analysis folder###
setwd("/Users/wksmith/Documents/GitHub/MSI_Plot")

#Packages####################################################################
library(tidyverse)
library(ggpubr)
library(RColorBrewer)

##########################################################################################################
#Read Salter
salter2023 = read_csv("CSV2023_forR/Salter_2023_overstory_clean.csv")
salter_PIPO_2023=subset(salter2023, Species=="PIPO")
#All Species
ggplot(salter2023, aes(x = DBH, fill = Species)) +
  geom_histogram(alpha=0.5, color='black',binwidth=1, position="identity") +
  facet_wrap(~Unit)+
  scale_y_continuous("Number of Trees") +
  scale_x_continuous("DBH (inch)") +
  theme_bw() +
  labs(fill = "Species",title = "") +
  theme(legend.text=element_text(size=20),legend.title=element_text(size=24)) +
  theme(legend.background = element_rect(linetype="solid", colour ="black"))+
  theme(text = element_text(size = 24))
ggsave(paste('Figures/Salter/Salter_DBH_2023.png',sep=''),dpi=300,width=250,height=200,units='mm')

#PIPO
ggplot(salter_PIPO_2023, aes(x = floor(DBH),fill=DIA_CLASS)) +
  geom_histogram(alpha=0.5, color='black',binwidth=1, position="stack") +
  scale_y_continuous("Number of Trees",expand = c(0, 0), limits = c(0, 30),breaks=c(0,5,10,15,20,25,30)) +
  scale_x_continuous("DBH (inch)",breaks=seq(0,40,5)) +
  scale_fill_manual(values=c('red','orange','yellow','green','blue','purple'),labels=c('< 12 inch', '12-16 inch', '16-20 inch','20-25 inch','25-27 inch','> 27 inch'))+
  theme_bw() +
  labs(fill = "Category",title = "") +
  theme(legend.position = c(.15,.8),legend.text=element_text(size=20),legend.title=element_text(size=24)) +
  theme(legend.background = element_rect(linetype="solid", colour ="black"))+
  theme(text = element_text(size = 24))
ggsave(paste('Figures/Salter/Salter_PIPO_DBH_2023.png',sep=''),dpi=300,width=250,height=200,units='mm')

ggplot(salter_PIPO_2023, aes(x = DBH)) +
  geom_histogram(alpha=0.5, color='black',binwidth=1, position="identity") +
  facet_wrap(~Unit)+
  scale_y_continuous("Number of Trees") +
  geom_vline(xintercept = c(12,16,20),linetype = "dashed",color='gray')+
  scale_x_continuous("DBH (inch)") +
  theme_bw() +
  labs(fill = "",title = "") +
  theme(legend.position = c(.8,.8),legend.text=element_text(size=20),legend.title=element_text(size=24)) +
  theme(legend.background = element_rect(linetype="solid", colour ="black"))+
  theme(text = element_text(size = 24))
ggsave(paste('Figures/Salter/Salter_PIPO_DBH_2023_byUnit.png',sep=''),dpi=300,width=250,height=200,units='mm')

#Summarize Beetle
salter_PIPO_Beetle_2023=subset(salter_PIPO_2023, !is.na(Beetle))
salter_PIPO_Beetle_2023$Beetle2 <- factor(salter_PIPO_Beetle_2023$Beetle,levels=c("D","GPT","G"))
ggplot(salter_PIPO_Beetle_2023, aes(x = floor(DBH), fill=Beetle2)) +
  geom_histogram(alpha=1, color='black',binwidth=1, position="stack") +
  scale_fill_manual(values=c("red","orange","green3"),labels=c('Dead', 'Pitch Tubes', 'Green'))+
  scale_y_continuous("Number of Trees",expand = c(0, 0), limits = c(0, 30),breaks=c(0,5,10,15,20,25,30)) +
  scale_x_continuous("DBH (inch)",breaks=seq(0,40,5)) +
  theme_bw() +
  labs(fill = "Category",title = "") +
  theme(legend.position = c(.15,.8),legend.text=element_text(size=20),legend.title=element_text(size=24)) +
  theme(legend.background = element_rect(linetype="solid", colour ="black"))+
  theme(text = element_text(size = 24))
ggsave(paste('Figures/Salter/Salter_PIPO_DBH_Beetle_2023.png',sep=''),dpi=300,width=250,height=200,units='mm')

#Summarize BA
salter_BA = salter_PIPO_2023 %>% group_by(Plot) %>% 
  summarise(N=n(),BA = sum(BA,na.rm=T)*10,Cat=NA)
salter_BA$Cat[which(salter_BA$BA>=120)]='Very High'
salter_BA$Cat[which(salter_BA$BA>=90 & salter_BA$BA<120)]='High'
salter_BA$Cat[which(salter_BA$BA>=70 & salter_BA$BA<90)]='Medium'
salter_BA$Cat[which(salter_BA$BA<70)]='Low'
salter_BA$Cat2 <- factor(salter_BA$Cat,levels=c("Very High","High","Medium","Low"))

ggplot(salter_BA, aes(x = BA,fill=Cat2)) +
  geom_histogram(alpha=0.9, color='black',breaks=seq(20,370,10),binwidth=10, position="stack") +
  scale_fill_manual(values=c("red","orange","yellow","green2"))+
  scale_y_continuous("Number of Plots",expand = c(0, 0), limits = c(0, 10),breaks=c(0,2,4,6,8,10)) +
  scale_x_continuous("Basal Area (sq. ft. per acre)",breaks=c(0,50,100,150,200,250,300,350)) +
  theme_bw() +
  labs(fill = "Vulnerability",title = "") +
  theme(legend.position = c(.8,.8),legend.text=element_text(size=20),legend.title=element_text(size=24)) +
  theme(legend.background = element_rect(linetype="solid", colour ="black"))+
  theme(text = element_text(size = 24))
ggsave(paste('Figures/Salter/Salter_PIPO_BA_2023_byPlot.png',sep=''),dpi=300,width=250,height=200,units='mm')

#Summarize Percent BA by Plot
salter_PIPO_A = subset(salter_PIPO_2023, DIA_CLASS == 'A')
salter_Stat_A = salter_PIPO_A %>% group_by(Plot) %>% 
  summarise(Cat='A',N=n(),DBH_mean = mean(DBH,na.rm=T),BA_sum = mean(PLOT_BA,na.rm=T), PER_BA_sum = sum(PER_BA,na.rm=T))
salter_PIPO_B = subset(salter_PIPO_2023, DIA_CLASS == 'B')
salter_Stat_B = salter_PIPO_B %>% group_by(Plot) %>% 
  summarise(Cat='B',N=n(),DBH_mean = mean(DBH,na.rm=T),BA_sum = mean(PLOT_BA,na.rm=T),PER_BA_sum = sum(PER_BA,na.rm=T))
salter_PIPO_C = subset(salter_PIPO_2023, DIA_CLASS == 'C')
salter_Stat_C = salter_PIPO_C %>% group_by(Plot) %>% 
  summarise(Cat='C',N=n(),DBH_mean = mean(DBH,na.rm=T),BA_sum = mean(PLOT_BA,na.rm=T),PER_BA_sum = sum(PER_BA,na.rm=T))
salter_PIPO_D = subset(salter_PIPO_2023, DIA_CLASS == 'D')
salter_Stat_D = salter_PIPO_D %>% group_by(Plot) %>% 
  summarise(Cat='D',N=n(),DBH_mean = mean(DBH,na.rm=T),BA_sum = mean(PLOT_BA,na.rm=T),PER_BA_sum = sum(PER_BA,na.rm=T))
salter_PIPO_E = subset(salter_PIPO_2023, DIA_CLASS == 'E')
salter_Stat_E = salter_PIPO_E %>% group_by(Plot) %>% 
  summarise(Cat='E',N=n(),DBH_mean = mean(DBH,na.rm=T),BA_sum = mean(PLOT_BA,na.rm=T),PER_BA_sum = sum(PER_BA,na.rm=T))
salter_PIPO_F = subset(salter_PIPO_2023, DIA_CLASS == 'F')
salter_Stat_F = salter_PIPO_F %>% group_by(Plot) %>% 
  summarise(Cat='F',N=n(),DBH_mean = mean(DBH,na.rm=T),BA_sum = mean(PLOT_BA,na.rm=T),PER_BA_sum = sum(PER_BA,na.rm=T))
#Combine
salter_Stat_All <- rbind(salter_Stat_A,salter_Stat_B,salter_Stat_C,salter_Stat_D,salter_Stat_E,salter_Stat_F)
salter_Stat_All$Plot2 <- paste0("Plot ",salter_Stat_All$Plot," (BA = ",round(salter_Stat_All$BA_sum,1),")")

ggplot(salter_Stat_All, aes(x = Cat, y = PER_BA_sum*100)) +
  geom_bar(stat="identity",position=position_dodge()) +
  facet_wrap(~Plot2)+
  geom_text(aes(label=paste("n=",N)), vjust=-0.1, color="black", size=4)+
  labs(y = "BA (percent)", x = "DBH Class (inch)")+
  scale_y_continuous(expand = c(0, 0),limits = c(0,105),breaks=c(0,20,40,60,80,100)) +
  scale_x_discrete(labels=c('< 12','12 to 16', '16 to 20','20 to 25','25 to 27','> 27'),guide = guide_axis(angle = 45))+
  theme_bw() +
  theme(text = element_text(size = 20))
ggsave(paste('Figures/Salter/Salter_PIPO_BApercent_DBHClasses_2023.png',sep=''),dpi=300,width=400,height=300,units='mm')

#Summarize DBH Class by Unit
salter_Stat_A = salter_PIPO_A %>% group_by(Unit) %>% 
  summarise(Cat='A',N=n(),DBH_mean = mean(DBH,na.rm=T),BA_sum = mean(PLOT_BA,na.rm=T))
salter_PIPO_B = subset(salter_PIPO_2023, DIA_CLASS == 'B')
salter_Stat_B = salter_PIPO_B %>% group_by(Unit) %>% 
  summarise(Cat='B',N=n(),DBH_mean = mean(DBH,na.rm=T),BA_sum = mean(PLOT_BA,na.rm=T))
salter_PIPO_C = subset(salter_PIPO_2023, DIA_CLASS == 'C')
salter_Stat_C = salter_PIPO_C %>% group_by(Unit) %>% 
  summarise(Cat='C',N=n(),DBH_mean = mean(DBH,na.rm=T),BA_sum = mean(PLOT_BA,na.rm=T))
salter_PIPO_D = subset(salter_PIPO_2023, DIA_CLASS == 'D')
salter_Stat_D = salter_PIPO_D %>% group_by(Unit) %>% 
  summarise(Cat='D',N=n(),DBH_mean = mean(DBH,na.rm=T),BA_sum = mean(PLOT_BA,na.rm=T))
salter_PIPO_E = subset(salter_PIPO_2023, DIA_CLASS == 'E')
salter_Stat_E = salter_PIPO_E %>% group_by(Unit) %>% 
  summarise(Cat='E',N=n(),DBH_mean = mean(DBH,na.rm=T),BA_sum = mean(PLOT_BA,na.rm=T))
salter_PIPO_F = subset(salter_PIPO_2023, DIA_CLASS == 'F')
salter_Stat_F = salter_PIPO_F %>% group_by(Unit) %>% 
  summarise(Cat='F',N=n(),DBH_mean = mean(DBH,na.rm=T),BA_sum = mean(PLOT_BA,na.rm=T))
#Combine
salter_Stat_All <- rbind(salter_Stat_A,salter_Stat_B,salter_Stat_C,salter_Stat_D,salter_Stat_E,salter_Stat_F)
ggplot(salter_Stat_All, aes(fill = Cat, x = Unit, y = N)) +
  geom_bar(position="stack", stat="identity",alpha=0.9, color='black') +
  labs(y = "Number of Trees", x = "Unit")+
  scale_y_continuous(expand = c(0, 0),limits = c(0,50),breaks=c(0,10,20,30,40,50)) +
  #scale_x_discrete(labels=c('< 12','12 to 16', '16 to 20','20 to 25','25 to 27','> 27'),guide = guide_axis(angle = 45))+
  scale_fill_manual(values=c('red','orange','yellow','green','blue','purple'),labels=c('< 12 inch', '12-16 inch', '16-20 inch','20-25 inch','25-27 inch','> 27 inch'))+
  theme_bw() +
  theme(text = element_text(size = 20))
ggsave(paste('Figures/Salter/Salter_PIPO_Trees_byClass_2023.png',sep=''),dpi=300,width=300,height=200,units='mm')

#Summarize Age
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
  geom_text(aes(label=paste("n=",N)), vjust=-1, color="black", size=4)+
  labs(y = "Age (years)", x = "DBH Class")+
  scale_y_continuous(limits = c(0,150)) +
  scale_x_discrete(labels=c('12 to 16 inch', '16 to 20 inch', '> 20 inch'),guide = guide_axis(angle = 45))+
  theme_bw() +
  theme(text = element_text(size = 20))
ggsave(paste('Figures/Salter/Salter_PIPO_DBH_Age_Classes_2023.png',sep=''),dpi=300,width=250,height=200,units='mm')

ggplot(salterStat_long, aes(x=DBH, y=Age)) +
  geom_point(size=3)+
  geom_smooth(method=lm, color="black")+
  stat_cor() +
  labs(x='DBH (inch)',y='Age (year)')+
  theme_bw()+
  theme(legend.position = c(0.8, 0.2),legend.title = element_blank(),legend.text=element_text(size=16)) +
  theme(text = element_text(size = 20))
ggsave(paste('Figures/Salter/Salter_PIPO_DBH_Age_Regression_2023.png',sep=''),dpi=300,width=250,height=200,units='mm')


