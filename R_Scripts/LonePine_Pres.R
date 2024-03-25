

#Before you begin####
#Create a LonePine_Analysis folder on your computer. Save all necessary csv files into the folder and set it as your working directory
setwd("C:/Users/Laura/Dropbox/LonePine_Analysis")


#Setup#######################################################################

library(tidyverse)
library(ggplot2)
library(gridExtra)


treatment = read_csv("treatments.csv")


##Overstory##################################################################
#Read in and clean up data

overstory2022 = read_csv("LonePine2022_overstory.csv")
overstory2022 = subset(overstory2022, Plot!="25")

Post_70BA = subset(overstory2022, Trt=="70BA")

overstory2020 = read_csv("Overstory2020_updated.csv")
overstory2020 = subset(overstory2020, Plot>=25, Plot<=66)
overstory2020 = subset(overstory2020, Block!="Brenna_thesis")
overstory2020 = subset(overstory2020, Plot!="25")
overstory2020 = merge(treatment, overstory2020, "Plot")

Pre_70BA = subset(overstory2020, Trt=="70BA")

####PIPO Sumstats####
#Summarizes total ponderosa basal area by plot and export .csv files 

overstorysum2020=subset(overstory2020, Species=="PIPO")
overstorysum2022=subset(overstory2022, Species=="PIPO")

Summary2020 = overstorysum2020 %>%
  group_by(Plot, Trt, Species) %>%
  summarise(BA = sum(BA)*10)
Summary2022 = overstorysum2022 %>%
  group_by(Plot, Trt, Species) %>%
  summarise(BA = sum(BA)*10)

write.csv(Summary2020,"C:/Users/Laura/Dropbox/LonePine_Analysis/QAQC\\overstorysum2020.csv", row.names = FALSE)
write.csv(Summary2022,"C:/Users/Laura/Dropbox/LonePine_Analysis/QAQC\\overstorysum2022.csv", row.names = FALSE)

###All Species####

#####BA x DBH####
#Creates and exports a size class distribution histogram

Pre70BA_S = ggplot(Pre_70BA, aes(x=DBH_in, fill=Species))+
  geom_histogram(binwidth = 1)+
  coord_cartesian(ylim=c(0,25), xlim=c(4,35))+
  labs(title="2020", x="Diameter (in)")+
  scale_fill_manual(values=species_palette)+
  theme_light()+
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 22, hjust = 0.5))

Post70BA_S = ggplot(Post_70BA, aes(x=DBH, fill=Species))+
  geom_histogram(binwidth = 1)+
  coord_cartesian(ylim=c(0,20), xlim=c(4,35))+
  labs(title="2022", x="Diameter (in)")+
  scale_fill_manual(values=species_palette)+
  theme_light()+
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 22, hjust = 0.5))

png(file="LonePine_Figs/Species_70.png", width=1200, height=600, res=100)
grid.arrange(Pre70BA_S, Post70BA_S, nrow=1, left="70 BA") 
dev.off()

###Just PIPO####
#Subsets only ponderosa

Pre_70BA = subset(Pre_70BA, Species=="PIPO")

Post_70BA = subset(Post_70BA, Species=="PIPO")

Pre_70BA$year = "pre"
Post_70BA$year = "post"

Pre_70BAsum = Pre_70BA %>%
  group_by(Plot, Trt, ObjectID) %>%
  summarise(Year=year,
            Height = Height,
            DBH = DBH_in, 
            BA = BA)
Post_70BAsum = Post_70BA %>%
  group_by(Plot, Trt, Tree) %>%
  summarise(Year=year, 
            Height = Height,
            DBH = DBH,
            BA = BA)

PIPO70_summary = rbind(Pre_70BAsum, Post_70BAsum)


#####DBH Histogram####
#Create and export size class distribution histograms


Pre70BA_PIPO = ggplot(Pre_70BAsum, aes(x=DBH))+
  geom_histogram(binwidth = 1, fill="forestgreen", color="darkgreen", size=2)+
  coord_cartesian(ylim=c(0,15), xlim=c(4,35))+
  scale_fill_manual(values=beetle_palette, labels=c("D" = "Dead", "DG" = "Dead w/ Galleries", "G" = "Green", "GPT" = "Green w/ Pitch Tubes", "BPT" = "Brown w/ Pitch Tubes"))+
  labs(title="2020", x="Diameter (in)", y="Tree Count")+
  guides(fill = guide_legend(title = "Beetle Evidence"))+
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18),
    plot.title = element_text(size = 20, hjust = 0.5))
Post70BA_PIPO = ggplot(Post_70BAsum, aes(x=DBH))+
  geom_histogram(binwidth = 1, fill="forestgreen", color="darkgreen", size=2)+
  coord_cartesian(ylim=c(0,15), xlim=c(4,35))+
  scale_fill_manual(values=beetle_palette, labels=c("D" = "Dead", "DG" = "Dead w/ Galleries", "G" = "Green", "GPT" = "Green w/ Pitch Tubes", "BPT" = "Brown w/ Pitch Tubes"))+
  labs(title="2022", x="Diameter (in)", y="Tree Count")+
  guides(fill = guide_legend(title = "Beetle Evidence"))+
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 18),
    plot.title = element_text(size = 20, hjust = 0.5))

png(file="LonePine_Figs/PIPO_DBH.png", width=1200, height=600, res=100)
grid.arrange(Pre70BA_PIPO, Post70BA_PIPO, nrow=1)
dev.off()

png(file="LonePine_Figs/PIPO_BAcomp.png", width=1800, height=1600, res=200)
ggplot(PIPO70_summary, aes(x=DBH, fill=Year, color=Year))+
  geom_histogram(alpha=0.4, position="identity", binwidth = 1)+
  scale_x_continuous(breaks = seq(from = 5, to = 30, by = 5))+
  scale_fill_manual(values = c("blue", "gold"))+
  scale_color_manual(values = c("darkblue", "darkorange"))+
  labs(x="DBH (in)", y="Number of Trees")+
  coord_cartesian(ylim=c(0,15), xlim=c(4,30))+
  theme_light()+
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 22),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18),
    plot.title = element_text(size = 22, hjust = 0.5),
    legend.position = c(.8,.8))
dev.off()
#BA Pre/Post Summaries without Control

TrtSummary2020 = subset(Summary2020, Trt!="Control")
TrtSummary2022 = subset(Summary2022, Trt!="Ctrl")
TrtSummary2020$year = "Pre"
TrtSummary2022$year = "Post"

TrtSummary = rbind(TrtSummary2020, TrtSummary2022)

png(file="LonePine_Figs/PrePostBA.png", width=1800, height=1600, res=200)
ggplot(TrtSummary, aes(x=BA, fill=year, color=year))+
  geom_histogram(alpha=0.4, position="identity", binwidth = 10)+
  scale_fill_manual(values = c("darkblue", "gold"))+
  scale_color_manual(values = c("black", "darkorange"))+
  labs(x="BA (ft^2/acre)", y="Number of Plots", fill = "Year", color = "Year")+
  coord_cartesian(ylim=c(0,5), xlim=c(0,300))+
  theme_light()+
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 22),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    plot.title = element_text(size = 22, hjust = 0.5),
    legend.position = c(.8,.8))

dev.off()

#####BA x DBH Bar Chart####

# bucketing values into bins

PIPO70_summary <- within(PIPO70_summary, {   
  DBHclass <- NA # need to initialize variable
  DBHclass[DBH < 1] <- "0"
  DBHclass[DBH >= 1 & DBH < 2] <- "1"
  DBHclass[DBH >= 1 & DBH < 3] <- "2"
  DBHclass[DBH >= 3 & DBH < 4] <- "3"
  DBHclass[DBH >= 4 & DBH < 5] <- "4"
  DBHclass[DBH >= 5 & DBH < 6] <- "5"
  DBHclass[DBH >= 6 & DBH < 7] <- "6"
  DBHclass[DBH >= 7 & DBH < 8] <- "7"
  DBHclass[DBH >= 8 & DBH < 9] <- "8"
  DBHclass[DBH >= 9 & DBH < 10] <- "9"
  DBHclass[DBH >= 10 & DBH < 11] <- "10"
  DBHclass[DBH >= 11 & DBH < 12] <- "11"
  DBHclass[DBH >= 12 & DBH < 13] <- "12"
  DBHclass[DBH >= 13 & DBH < 14] <- "13"
  DBHclass[DBH >= 14 & DBH < 15] <- "14"
  DBHclass[DBH >= 15 & DBH < 16] <- "15"
  DBHclass[DBH >= 16 & DBH < 17] <- "16"
  DBHclass[DBH >= 17 & DBH < 18] <- "17"
  DBHclass[DBH >= 18 & DBH < 19] <- "18"
  DBHclass[DBH >= 19 & DBH < 20] <- "19"
  DBHclass[DBH >= 20 & DBH < 21] <- "20"
  DBHclass[DBH >= 21 & DBH < 22] <- "21"
  DBHclass[DBH >= 22 & DBH < 23] <- "22"
  DBHclass[DBH >= 23 & DBH < 24] <- "23"
  DBHclass[DBH >= 24 & DBH < 25] <- "24"
  DBHclass[DBH >= 25 & DBH < 26] <- "25"
  DBHclass[DBH >= 26 & DBH < 27] <- "26"
  DBHclass[DBH >= 27 & DBH < 28] <- "27"
  } )


PIPO70_DBHclass = PIPO70_summary %>%
  group_by(DBHclass, Year) %>%
  summarise(DBHclass=DBHclass,
            Year = Year,
            BA = sum(BA))
PIPO70_DBHclass$DBHclass = as.numeric(PIPO70_DBHclass$DBHclass)

PIPO70_DBHclass = unique(PIPO70_DBHclass)

png(file="LonePine_Figs/70BAvsDBH.png", width=2000, height=1600, res=200)
ggplot(PIPO70_DBHclass, aes(y=BA, x=DBHclass, fill=Year, color=Year))+
  geom_col(alpha=0.4, position="identity")+
  scale_fill_manual(values = c("darkblue", "gold"))+
  scale_color_manual(values = c("black", "darkorange"))+
  labs(x="DBH (in)", y="BA (ft^2")+
  coord_cartesian(ylim=c(0,20), xlim = c(4,30))+
  theme_light()+
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 22, hjust = 0.5))

dev.off()


#####Beetle Histogram####
#Create and export ponderosa size class distribution histograms - color-coded by beetle evidence

beetle_palette = c("D" = "grey70", "DG" = "grey20", "G" = "forestgreen", "GPT" = "olivedrab3", "BPT" = "coral4")

Pre70BA = ggplot(Pre_70BA, aes(x=DBH_in, fill=Beetle), color="black")+
  geom_histogram(binwidth = 1)+
  coord_cartesian(ylim=c(0,15), xlim=c(4,30))+
  scale_fill_manual(values=beetle_palette, labels=c("D" = "Dead", "DG" = "Dead w/ Galleries", "G" = "Green", "GPT" = "Green w/ Pitch Tubes", "BPT" = "Brown w/ Pitch Tubes"))+
  labs(title="2020", x="Diameter (in)")+
  guides(fill = guide_legend(title = "Beetle Evidence"))+
  theme_light()+
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 24, hjust = 0.5),
    legend.position = c(.75,.8))
    
Post70BA = ggplot(Post_70BA, aes(x=DBH, fill=Beetle), color="black")+
  geom_histogram(binwidth = 1, show.legend=F)+
  coord_cartesian(ylim=c(0,15), xlim=c(4,30))+
  scale_fill_manual(values=beetle_palette, labels=c("D" = "Dead", "DG" = "Dead w/ Galleries", "G" = "Green", "GPT" = "Green w/ Pitch Tubes", "BPT" = "Brown w/ Pitch Tubes"))+
  labs(title="2022", x="Diameter (in)")+
  guides(fill = guide_legend(title = "Beetle Evidence"))+
  theme_light()+
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 24, hjust = 0.5))

png(file="LonePine_Figs/BeetleEvidence70.png", width=1200, height=2400, res=200)
grid.arrange(Pre70BA, Post70BA, ncol=1)
dev.off()

##Canopy#####################################################################
#Import and clean up canopy data

canopy2020 = read_csv("Lone_Pine_Canopy.csv")
canopy2020 = subset(canopy2020, Plot>=25)
canopy2020 = subset(canopy2020,Plot<=66)
canopy2020 = subset(canopy2020, Block!="Brenna_thesis")
canopy2020 = merge(treatment, canopy2020, "Plot")
canopy2020 = subset(canopy2020, Plot!=31)
canopy2020 = subset(canopy2020, Species!="AMAL")
canopy2020 = subset(canopy2020, Species!="NONE")

canopy2022 = read_csv("LonePine2022_canopy.csv")
canopy2022 = merge(treatment, canopy2022, "Plot")
canopy2022 = subset(canopy2022, Plot!=31)


###Percent Canopy by Species####
#Summarize percent cover by species per plot

canopy_summary2020 = canopy2020 %>%
  group_by(Plot, Trt, Species) %>%
  summarise(Per_Cover = sum(PercentCover))

canopy_summary2022 = canopy2022 %>%
  group_by(Plot, Trt, Species) %>%
  summarise(Hits = n(),
            Per_Cover = n()*100/30)


canopy_summary2022 = canopy_summary2022 %>%
  group_by(Trt, Species) %>%
  summarise(Per_Cover = sum(Per_Cover)/40)

canopy_summary2020 = canopy_summary2020 %>%
  group_by(Trt, Species) %>%
  summarise(Per_Cover = sum(Per_Cover)/40)
canopy_summary2022 = na.omit(canopy_summary2022)

#plot and export bar charts illustrating average percent canopy cover by species
Can_20=ggplot(canopy_summary2020)+
  geom_col(aes(x=Trt, y=Per_Cover, fill=Species), position="dodge")+
  coord_cartesian(ylim=c(0,15))+
  labs(title="2020", x="Prescription", y="% Cover")+
  theme_light()+
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 22, hjust = 0.5),
    legend.position = c(.7,.9))
  
Can_22=ggplot(canopy_summary2022)+
  geom_col(aes(x=Trt, y=Per_Cover, fill=Species), position="dodge")+
  coord_cartesian(ylim=c(0,15))+
  labs(title="2022", x="Prescription", y="% Cover")+
  theme_light()+
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 22, hjust = 0.5),
    legend.position = 'none' )

png(file="LonePine_Figs/Canopy.png", width=1200, height=2400, res=200)
grid.arrange(Can_20, Can_22, nrow=2)
dev.off()
  
##Shrubs######################################################################
#Import and clean shrub cover data

shrubs2020 = read_csv("LonePine2020_Shrubs.csv")
shrubs2020 = merge(treatment, shrubs2020, "Plot")
shrubs2020 = subset(shrubs2020, Plot>=25)
shrubs2020 = subset(shrubs2020, Plot<=66)
shrubs2020 = subset(shrubs2020, Block!="Brenna_thesis")

shrubs2022 = read_csv("LonePine2022_shrubs.csv")
shrubs2022 = merge(treatment, shrubs2022, "Plot")


###Percent Cover by Species####

#Summarize percent cover by plot, species, and height class

shrubsum2020 = shrubs2020 %>%
  group_by(Plot, Trt, Species) %>%
  summarise(Length_m = sum(Length_m))
shrubsum2020 = shrubsum2020 %>%
  group_by(Trt, Species) %>%
  summarise(Per_Cover = mean(Length_m)/30*100)

shrubsum2022 = shrubs2022 %>%
  group_by(Plot, Trt, Species) %>%
  summarise(Length_m = sum(Length_m))
shrubsum2022 = shrubsum2022 %>%
  group_by(Trt, Species,) %>%
  summarise(Per_Cover = mean(Length_m)/30*100)


shrubht2020 = shrubs2020 %>%
  group_by(Plot, Trt, Species, HeightClass) %>%
  summarise(Length_m = sum(Length_m))
shrubht2020 = shrubht2020 %>%
  group_by(Trt, Species, HeightClass) %>%
  summarise(Per_Cover = mean(Length_m)/30*100)

shrubht2022 = shrubs2022 %>%
  group_by(Plot, Trt, Species, Above_BH) %>%
  summarise(Length_m = sum(Length_m))
shrubht2022 = shrubht2022 %>%
  group_by(Trt, Species,Above_BH) %>%
  summarise(Per_Cover = mean(Length_m)/30*100)

#separate shrubs by height class

shrubsumA2020 = subset(shrubht2020, HeightClass!="b")
shrubsumA2022 = subset(shrubht2022, Above_BH="Yes")
shrubsumB2020 = subset(shrubht2020, HeightClass!="a")
shrubsumB2022 = subset(shrubht2022, Above_BH!="Yes")

#Create and export average percent cover by shrub species

shrub_pal = c("ACGL" = "red3","AMAL2" = "orange2","PRVI" = "yellow3", "QUGA" ='green4',"ROWO" = 'blue3',"SYRO" = 'purple3')

A20=ggplot(shrubsumA2020)+
  geom_col(aes(x=Trt, y=Per_Cover, fill=Species))+
  coord_cartesian(ylim=c(0,60))+
  scale_fill_manual(values = shrub_pal)+
  labs(y="% Cover", x="Prescription")+
  theme_light()+
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 22, hjust = 0.5),
    legend.position = 'none')

A22=ggplot(shrubsumA2022)+
  geom_col(aes(x=Trt, y=Per_Cover, fill=Species))+
  coord_cartesian(ylim=c(0,60))+
  scale_fill_manual(values = shrub_pal)+
  labs(y="% Cover", x="Prescription")+
  theme_light()+
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 22, hjust = 0.5),
    legend.position = 'none')

B20=ggplot(shrubsumB2020)+
  geom_col(aes(x=Trt, y=Per_Cover, fill=Species))+
  coord_cartesian(ylim=c(0,60))+
  scale_fill_manual(values = shrub_pal, guide = guide_legend(nrow=2))+
  labs(y="% Cover", x="Prescription")+
  theme_light()+
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 22, hjust = 0.5),
    legend.position = c(.5,.85))

B22=ggplot(shrubsumB2022)+
  geom_col(aes(x=Trt, y=Per_Cover, fill=Species))+
  coord_cartesian(ylim=c(0,60))+
  scale_fill_manual(values = shrub_pal)+
  labs(y="% Cover", x="Prescription")+
  theme_light()+
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 22, hjust = 0.5),
    legend.position = 'none')

Shrub20 = ggplot(shrubsum2020)+
  geom_col(aes(x=Trt, y=Per_Cover, fill=Species))+
  coord_cartesian(ylim=c(0,100))+
  scale_fill_manual(values = shrub_pal)+
  labs(title="2020", x="Prescription", y="% Cover")+
  theme_light()+
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 22, hjust = 0.5),
    legend.position = 'none')

Shrub22 = ggplot(shrubsum2022)+
  geom_col(aes(x=Trt, y=Per_Cover, fill=Species))+
  coord_cartesian(ylim=c(0,100))+
  scale_fill_manual(values = shrub_pal)+
  labs(title="2022",x="Prescription", y="% Cover")+
  theme_light()+
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 14),
    plot.title = element_text(size = 22, hjust = 0.5),
    legend.position = c(.7,.8))
  


png(file="LonePine_Figs/Shrub_Cover_HT.png", width=2000, height=2000, res=200)
grid.arrange(A20, B20, A22, B22, nrow=2)
dev.off()

png(file="LonePine_Figs/Shrub_Cover.png", width=2000, height=1200, res=200)
grid.arrange(Shrub20, Shrub22, nrow=1)
dev.off()

##Regeneration################################################################

regeneration2022 = read_csv("LonePine2022_Regen.csv")
regeneration2022 = merge(treatment, regeneration2022, "Plot")
regeneration2022 = subset(regeneration2022, Species!="SYLA")
regeneration2022$Density = as.numeric(regeneration2022$Density)


###Frequency?####

regensummary2020 = regeneration2020 %>%
  group_by(Trt, Species, HeightClass) %>%
  summarise(Density = mean(total_seed_count))

regensummary2022 = regeneration2022 %>%
  group_by(Trt, Species, Above_BH) %>%
  summarise(Density = mean(Density))

Regen2020A = ggplot(subset(regensummary2020, HeightClass=="A"))+
  geom_col(aes(x=Trt, y=Density, fill=Species))+
  coord_cartesian(ylim=c(0,125))
Regen2020B = ggplot(subset(regensummary2020, HeightClass=="B"))+
  geom_col(aes(x=Trt, y=Density, fill=Species))+
  coord_cartesian(ylim=c(0,125))
Regen2022A = ggplot(subset(regensummary2022, Above_BH=="A"))+
  geom_col(aes(x=Trt, y=Density, fill=Species))+
  coord_cartesian(ylim=c(0,125))
Regen2022B = ggplot(subset(regensummary2022, Above_BH=="B"))+
  geom_col(aes(x=Trt, y=Density, fill=Species))+
  coord_cartesian(ylim=c(0,125))


grid.arrange(Regen2020A, Regen2022A, Regen2020B, Regen2022B, nrow=2)
grid.arrange(Regen2022A, Regen2022B, nrow=1)


##Quadrats####################################################################

quadrats2020 = read_csv("LonePine2020_quadrats.csv")
quadrats2020 = subset(quadrats2020, Plot>=25)
quadrats2020 = subset(quadrats2020, Plot<=66)
quadrats2020 = subset(quadrats2020, Block!="brenna_project")
quadrats2020 = subset(quadrats2020, Block!="Bg")
quadrats2020 = subset(quadrats2020, Block!="BG")

quadrats2020[is.na(quadrats2020)] = 0

quadrats2022 = read_csv("LonePine2022_quadrats.csv")
quadrats2022 = merge(treatment, quadrats2022, "Plot")
quadrats2022[is.na(quadrats2022)] = 0

###Fuels####
Fuel_2020 = quadrats2020 %>%
  group_by(Plot, Trt) %>%
  summarise(hr1 = mean(hr1)*4.0469,
            hr10 = mean(hr10)*4.0469,
            hr100 = mean(hr100)*4.0469)
Fuel_2020 = Fuel_2020 %>%
  group_by(Trt) %>%
  summarise(hr1 = mean(hr1),
            hr10 = mean(hr10),
            hr100 = mean(hr100))

Fuel_2022 = quadrats2022 %>%
  group_by(Plot, Trt) %>%
  summarise(hr1 = mean(hr1),
            hr10 = mean(hr10),
            hr100 = mean(hr100))
Fuel_2022 = Fuel_2022 %>%
  group_by(Trt) %>%
  summarise(hr1 = mean(hr1),
            hr10 = mean(hr10),
            hr100 = mean(hr100))

Fuel_2020 <- pivot_longer(Fuel_2020, cols=2:4, names_to = "SizeClass", values_to = "Tons_ac")

Fuel_2022 <- pivot_longer(Fuel_2022, cols=2:4, names_to = "SizeClass", values_to = "Tons_ac")


fuel_palette = c("hr1" = "yellow3", "hr10" = "orange3", "hr100" = "red4")

Fuels_2020 = ggplot(Fuel_2020, aes(fill=SizeClass, y=Tons_ac, x=Trt)) + 
  geom_col(show.legend=F)+
  coord_cartesian(ylim=c(0,4))+
  scale_fill_manual(values=fuel_palette, labels=c("1 hr", "10 hr", "100 hr"))+
  labs(title="2020", y="Tons/acre", x="Prescription")+
  theme_light()+
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 22, hjust = 0.5),
    legend.position = 'none')
Fuels_2022 = ggplot(Fuel_2022, aes(fill=SizeClass, y=Tons_ac, x=Trt)) + 
  geom_col()+
  coord_cartesian(ylim=c(0,4))+
  scale_fill_manual(values=fuel_palette, labels=c("1 hr", "10 hr", "100 hr"))+
  labs(title="2022", y="Tons/acre", x="Prescription")+
  theme_light()+
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 22, hjust = 0.5),
    legend.position = c(.7,.8))

png(file="LonePine_Figs/Fuels.png", width=2200, height=1200, res=200)
grid.arrange(Fuels_2020, Fuels_2022, ncol=2)
dev.off()

###Ground Cover####

GroundCover_2020 = quadrats2020 %>%
  group_by(Plot, Trt) %>%
  summarise(BareGround = mean(BareGround),
            LitterDuff = mean(LitterDuff),
            Rock = mean(Rock),
            Gravel = mean(Gravel),
            LichenMoss = mean(LichenMoss), 
            FWD = mean(FWD),
            CWD = mean(CWD), 
            WoodyBasal = mean(WoodyBasal))
GroundCover_2020 = GroundCover_2020 %>%
  group_by(Trt) %>%
  summarise(BareGround = mean(BareGround),
            LitterDuff = mean(LitterDuff),
            Rock = mean(Rock),
            Gravel = mean(Gravel),
            LichenMoss = mean(LichenMoss), 
            FWD = mean(FWD),
            CWD = mean(CWD),
            WoodyBasal = mean(WoodyBasal))

GroundCover_2022 = quadrats2022 %>%
  group_by(Plot, Trt) %>%
  summarise(BareGround = mean(Bare_Ground),
            LitterDuff = mean(LitterDuff),
            Rock = mean(Rock),
            Gravel = mean(Gravel),
            LichenMoss = mean(LichenMoss), 
            FWD = mean(FWD),
            CWD = mean(CWD), 
            WoodyBasal = mean(Woody_Basal))
GroundCover_2022 = GroundCover_2022 %>%
  group_by(Trt) %>%
  summarise(BareGround = mean(BareGround),
            LitterDuff = mean(LitterDuff),
            Rock = mean(Rock),
            Gravel = mean(Gravel),
            LichenMoss = mean(LichenMoss), 
            FWD = mean(FWD),
            CWD = mean(CWD),
            WoodyBasal = mean(WoodyBasal))

GroundCover_2020 <- pivot_longer(GroundCover_2020, cols=2:9, names_to = "CoverType", values_to = "Per_Cover")

GroundCover_2022 <- pivot_longer(GroundCover_2022, cols=2:9, names_to = "CoverType", values_to = "Per_Cover")

ground_palette = c("BareGround" = "firebrick", "CWD" = "darkorange", "FWD" = "goldenrod1", "Gravel" = "grey80" , "LichenMoss" = "seagreen3", "LitterDuff" = "steelblue4", "Rock" = "grey30",  "WoodyBasal" = "orchid4")

Ground_2020 = ggplot(GroundCover_2020, aes(fill=CoverType, y=Per_Cover, x=Trt)) + 
  geom_col(show.legend=F)+
  coord_cartesian(ylim=c(0,100))+
  scale_fill_manual(values=ground_palette, labels=c("BareGround" = "Bare Ground", "CWD" = "Coarse Woody Debris", "FWD" = "Fine Woody Debris", "LichenMoss" = "Lichen/Moss", "LitterDuff" = "Litter/Duff", "WoodyBasal" = "Woody Basal"))+
  labs(title="2020", y="% Cover", x="Prescription")+
  theme_light()+
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 22, hjust = 0.5),
    legend.position = c(.7,.8))
Ground_2022 = ggplot(GroundCover_2022, aes(fill=CoverType, y=Per_Cover, x=Trt)) + 
  geom_col()+
  coord_cartesian(ylim=c(0,100))+
  scale_fill_manual(values=ground_palette, labels=c("BareGround" = "Bare Ground", "CWD" = "Coarse Woody Debris", "FWD" = "Fine Woody Debris", "LichenMoss" = "Lichen/Moss", "LitterDuff" = "Litter/Duff", "WoodyBasal" = "Woody Basal"))+
  labs(title="2022", y="% Cover", x="Prescription")+
  theme_light()+
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 22, hjust = 0.5))

png(file="LonePine_Figs/GroundCover.png", width=3360, height=1400, res=200)
grid.arrange(Ground_2020, Ground_2022, ncol=2, layout_matrix = rbind(c(1,1,1,1,1,1,2,2,2,2,2,2,2,2,2)))
dev.off()


###Functional Groups####

FGColors =  c("Gramminoid" = "slategray3", "Forb" = "orchid4", "Shrub" = "forestgreen")

Funct_2020 = quadrats2020 %>%
  group_by(Plot, Trt) %>%
  summarise(Gramminoids = mean(Graminoids),
            Forbs = mean(Forbs),
            Shrubs = mean(Shrubs),
            Seedlings = mean(Seedlings))
Funct_2020 = Funct_2020 %>%
  group_by(Trt) %>%
  summarise(Gramminoid = mean(Gramminoids),
            Forb = mean(Forbs),
            Shrub = mean(Shrubs),
            Seedling = mean(Seedlings))


Funct_2022 = quadrats2022 %>%
  group_by(Plot, Trt) %>%
  summarise(Gramminoids = mean(Graminoinds),
            Forbs = mean(Forbs),
            Shrubs = mean(Shrubs),
            Seedlings = mean(Seedlings))
Funct_2022 = Funct_2022 %>%
  group_by(Trt) %>%
  summarise(Gramminoid = mean(Gramminoids),
            Forb = mean(Forbs),
            Shrub = mean(Shrubs),
            Seedling = mean(Seedlings))

Funct_2020 <- pivot_longer(Funct_2020, cols=2:4, names_to = "FunctionalGroup", values_to = "Per_Cover")

Funct_2022 <- pivot_longer(Funct_2022, cols=2:4, names_to = "FunctionalGroup", values_to = "Per_Cover")


FG_2020 = ggplot(Funct_2020, aes(fill=FunctionalGroup, y=Per_Cover, x=Trt)) + 
  geom_col()+
  scale_fill_manual(values=FGColors)+
  coord_cartesian(ylim=c(0,50))+
  labs(title="2020", y="% Cover", x="Prescription")+
  theme_light()+
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 22, hjust = 0.5),
    legend.position = 'none')
FG_2022 = ggplot(Funct_2022, aes(fill=FunctionalGroup, y=Per_Cover, x=Trt)) + 
  geom_col()+
  scale_fill_manual(values=FGColors)+
  coord_cartesian(ylim=c(0,50))+
  labs(title="2022", y="% Cover", x="Prescription")+
  guides(fill=guide_legend(title="Functional Group"))+
  theme_light()+
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 22, hjust = 0.5),
    legend.position = c(.7,.85))


png(file="LonePine_Figs/FunctionalGroup.png", width=1200, height=2400, res=200)
grid.arrange(FG_2020, FG_2022, ncol=1)
dev.off()
