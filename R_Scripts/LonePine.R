### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
###                                                             ###
###                     BEST Plot Analysis                      ###
###                                                             ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

#To Do:
#Percent Cover
#Clean Up Beetle Charts
#Size Class by Species
#



library(tidyverse)
library(ggplot2)
library(gridExtra)

setwd("C:/Users/Laura/Dropbox/LonePine_Analysis")

treatment = read_csv("treatments.csv")


#Overstory####

overstory2022 = read_csv("LonePine2022_overstory.csv")
overstory2022 = subset(overstory2022, Plot!="25")

Post_70BA = subset(overstory2022, Trt=="70BA")
Post_90BA = subset(overstory2022, Trt=="90BA")
Post_Ctrl = subset(overstory2022, Trt=="Ctrl")

overstory2020 = read_csv("Overstory2020_updated.csv")
overstory2020 = subset(overstory2020, Plot>=25, Plot<=66)
overstory2020 = subset(overstory2020, Block!="Brenna_thesis")
overstory2020 = subset(overstory2020, Plot!="25")
overstory2020 = merge(treatment, overstory2020, "Plot")

Pre_70BA = subset(overstory2020, Trt=="70BA")
Pre_90BA = subset(overstory2020, Trt=="90BA")
Pre_Ctrl = subset(overstory2020, Trt=="Control")


##Size Distribution (Species)####

species_palette = c( "steelblue", "darkgreen", "goldenrod2")

Pre70BA_S = ggplot(Pre_70BA, aes(x=DBH_in, fill=Species))+
  geom_histogram(binwidth = 1)+
  coord_cartesian(ylim=c(0,25), xlim=c(4,35))+
  labs(title="2020", x="Diameter (in)")+
  scale_fill_manual(values=species_palette)+
  theme(axis.text = element_text(size = 10))+
  theme(legend.text = element_text(size = 15))+
  theme(plot.title = element_text(size = 20))

Pre90BA_S = ggplot(Pre_90BA, aes(x=DBH_in, fill=Species))+
  geom_histogram(binwidth = 1)+
  coord_cartesian(ylim=c(0,25), xlim=c(4,35))+
  labs(x="Diameter (in)")+
  scale_fill_manual(values=species_palette[-1])

PreCtrl_S = ggplot(Pre_Ctrl, aes(x=DBH_in, fill=Species))+
  geom_histogram(binwidth = 1)+
  coord_cartesian(ylim=c(0,20), xlim=c(4,35))+
  labs(x="Diameter (in)")+
  scale_fill_manual(values=species_palette)


Post70BA_S = ggplot(Post_70BA, aes(x=DBH, fill=Species))+
  geom_histogram(binwidth = 1)+
  coord_cartesian(ylim=c(0,20), xlim=c(4,35))+
  labs(title="2022", x="Diameter (in)")+
  scale_fill_manual(values=species_palette)+
  theme(axis.text = element_text(size = 10))+
  theme(legend.text = element_text(size = 15))+
  theme(plot.title = element_text(size = 20))

Post90BA_S = ggplot(Post_90BA, aes(x=DBH, fill=Species))+
  geom_histogram(binwidth = 1)+
  coord_cartesian(ylim=c(0,25), xlim=c(4,35))+
  labs(x="Diameter (in)")+
  scale_fill_manual(values=species_palette[-1])

PostCtrl_S = ggplot(Post_Ctrl, aes(x=DBH, fill=Species))+
  geom_histogram(binwidth = 1)+
  coord_cartesian(ylim=c(0,25), xlim=c(4,35))+
  labs(x="Diameter (in)")+
  scale_fill_manual(values=species_palette)

grid.arrange(Pre70BA_S, Post70BA_S, nrow=1, left="70 BA") 
grid.arrange(Pre90BA_S, Post90BA_S, nrow=1, left="90 BA")
C = grid.arrange(PreCtrl_S, PostCtrl_S, nrow=1, left="Control")

png(file="LonePine_Figs/Species_70.png", width=1200, height=600, res=100)
grid.arrange(Pre70BA_S, Post70BA_S, nrow=1, left="70 BA") 
dev.off()



##Size Distribution (Beetle)####

Pre_70BA = subset(Pre_70BA, Species=="PIPO")
Pre_90BA = subset(Pre_90BA, Species=="PIPO")
Pre_Ctrl = subset(Pre_Ctrl, Species=="PIPO")

Post_70BA = subset(Post_70BA, Species=="PIPO")
Post_90BA = subset(Post_90BA, Species=="PIPO")
Post_Ctrl = subset(Post_Ctrl, Species=="PIPO")

beetle_palette = c("D" = "grey70", "DG" = "grey20", "G" = "forestgreen", "GPT" = "olivedrab3", "BPT" = "coral4")

Pre70BA = ggplot(Pre_70BA, aes(x=DBH_in, fill=Beetle))+
  geom_histogram(binwidth = 1)+
  coord_cartesian(ylim=c(0,15), xlim=c(4,35))+
  scale_fill_manual(values=beetle_palette, labels=c("D" = "Dead", "DG" = "Dead w/ Galleries", "G" = "Green", "GPT" = "Green w/ Pitch Tubes", "BPT" = "Brown w/ Pitch Tubes"))+
  labs(title="2020", x="Diameter (in)")+
  guides(fill = guide_legend(title = "Beetle Evidence"))

Pre90BA = ggplot(Pre_90BA, aes(x=DBH_in, fill=Beetle))+
  geom_histogram(binwidth = 1)+
  coord_cartesian(ylim=c(0,15), xlim=c(4,35))+
  scale_fill_manual(values=beetle_palette, labels=c("D" = "Dead", "DG" = "Dead w/ Galleries", "G" = "Green", "GPT" = "Green w/ Pitch Tubes", "BPT" = "Brown w/ Pitch Tubes"))+
  labs(title=" ",x="Diameter (in)")+
  guides(fill = guide_legend(title = "Beetle Evidence"))

PreCtrl = ggplot(Pre_Ctrl, aes(x=DBH_in, fill=Beetle))+
  geom_histogram(binwidth = 1)+
  coord_cartesian(ylim=c(0,17), xlim=c(4,35))+
  scale_fill_manual(values=beetle_palette, labels=c("D" = "Dead", "DG" = "Dead w/ Galleries", "G" = "Green", "GPT" = "Green w/ Pitch Tubes", "BPT" = "Brown w/ Pitch Tubes"))+
  labs(title=" ",x="Diameter (in)")+
  guides(fill = guide_legend(title = "Beetle Evidence"))



Post70BA = ggplot(Post_70BA, aes(x=DBH, fill=Beetle))+
  geom_histogram(binwidth = 1)+
  coord_cartesian(ylim=c(0,15), xlim=c(4,35))+
  scale_fill_manual(values=beetle_palette, labels=c("D" = "Dead", "DG" = "Dead w/ Galleries", "G" = "Green", "GPT" = "Green w/ Pitch Tubes", "BPT" = "Brown w/ Pitch Tubes"))+
  labs(title="2022", x="Diameter (in)")+
  guides(fill = guide_legend(title = "Beetle Evidence"))

Post90BA = ggplot(Post_90BA, aes(x=DBH, fill=Beetle))+
  geom_histogram(binwidth = 1)+
  coord_cartesian(ylim=c(0,15), xlim=c(4,35))+
  scale_fill_manual(values=beetle_palette, labels=c("D" = "Dead", "DG" = "Dead w/ Galleries", "G" = "Green", "GPT" = "Green w/ Pitch Tubes", "BPT" = "Brown w/ Pitch Tubes"))+
  labs(title=" ",x="Diameter (in)")+
  guides(fill = guide_legend(title = "Beetle Evidence"))

PostCtrl = ggplot(Post_Ctrl, aes(x=DBH, fill=Beetle))+
  geom_histogram(binwidth = 1)+
  coord_cartesian(ylim=c(0,17), xlim=c(4,35))+
  scale_fill_manual(values=beetle_palette, labels=c("D" = "Dead", "DG" = "Dead w/ Galleries", "G" = "Green", "GPT" = "Green w/ Pitch Tubes", "BPT" = "Brown w/ Pitch Tubes"))+
  labs(title=" ",x="Diameter (in)")+
  guides(fill = guide_legend(title = "Beetle Evidence"))


A1 = grid.arrange(Pre70BA, Post70BA, nrow=1)
B2 = grid.arrange(Pre90BA, Post90BA, nrow=1, left="90 BA")
C3 = grid.arrange(PreCtrl, PostCtrl, nrow=1, left="Control")

png(file="LonePine_Figs/BeetleEvidence70.png", width=1200, height=600, res=100)
grid.arrange(Pre70BA, Post70BA, nrow=1)
dev.off()
#######################
Pre_70BA$year = "pre"
Post_70BA$year = "post"

Pre_70BA = Pre_70BA %>%
  group_by(Plot, Trt, ObjectID) %>%
  summarise(Year=year,
            Height = Height,
            DBH = DBH_in)
Post_70BA = Post_70BA %>%
  group_by(Year=year, 
           Plot, Trt, Tree) %>%
  summarise(Height = Height,
            DBH = DBH)

PIPO70_summary = rbind(Pre_70BA, Post_70BA)

png(file="LonePine_Figs/PIPO_BAcomp.png", width=1000, height=800, res=100)
ggplot(PIPO70_summary, aes(x=DBH, fill=Year, color=Year))+
  geom_histogram(alpha=0.4, position="identity", binwidth = 1)+
  scale_fill_manual(values = c("blue", "gold"))+
  scale_color_manual(values = c("darkblue", "darkorange"))+
  labs(x="DBH (in)", y="Number of Trees")+
  coord_cartesian(ylim=c(0,15), xlim=c(4,30))
  
dev.off()

##################
Pre70BA_PIPO = ggplot(Pre_70BA, aes(x=DBH_in))+
  geom_histogram(binwidth = 1, fill="forestgreen", color="darkgreen", size=2)+
  coord_cartesian(ylim=c(0,15), xlim=c(4,35))+
  scale_fill_manual(values=beetle_palette, labels=c("D" = "Dead", "DG" = "Dead w/ Galleries", "G" = "Green", "GPT" = "Green w/ Pitch Tubes", "BPT" = "Brown w/ Pitch Tubes"))+
  labs(title="2020", x="Diameter (in)")+
  guides(fill = guide_legend(title = "Beetle Evidence"))
Post70BA_PIPO = ggplot(Post_70BA, aes(x=DBH))+
  geom_histogram(binwidth = 1, fill="forestgreen", color="darkgreen", size=2)+
  coord_cartesian(ylim=c(0,15), xlim=c(4,35))+
  scale_fill_manual(values=beetle_palette, labels=c("D" = "Dead", "DG" = "Dead w/ Galleries", "G" = "Green", "GPT" = "Green w/ Pitch Tubes", "BPT" = "Brown w/ Pitch Tubes"))+
  labs(title="2022", x="Diameter (in)")+
  guides(fill = guide_legend(title = "Beetle Evidence"))

png(file="LonePine_Figs/PIPO_DBH.png", width=1200, height=600, res=100)
grid.arrange(Pre70BA_PIPO, Post70BA_PIPO, nrow=1)
dev.off()

Pre70BA_H = ggplot(Pre_70BA, aes(x=Height))+
  geom_histogram(binwidth = 3)+
  coord_cartesian(ylim=c(0,20), xlim=c(0,100))+
  labs(title="2020", x="Height (ft)")+
  scale_fill_manual(values=species_palette)

Post70BA_H = ggplot(Post_70BA, aes(x=Height))+
  geom_histogram(binwidth = 3)+
  coord_cartesian(ylim=c(0,20), xlim=c(0,100))+
  labs(title="2022", x="Height (ft)")+
  scale_fill_manual(values=species_palette)

png(file="LonePine_Figs/HeightClass.png", width=1000, height=600, res=100)
grid.arrange(Pre70BA_H, Post70BA_H, nrow=1)
dev.off()

##Summary Stats####


tree_summary2020 = overstory2020 %>%
  group_by(Plot, Trt) %>%
  summarise(TPA = n()*10, 
            BA = mean(0.00545415*DBH_in^2) * n() * 10, 
            QMD = sqrt(mean(0.00545415*DBH^2)/0.00545415))

tree_summary2022 = overstory2022 %>%
  group_by(Plot, Trt) %>%
  summarise(TPA = n()*10, 
            BA = mean(0.00545415*DBH^2) * n() * 10, 
            QMD = sqrt(mean(0.00545415*DBH^2)/0.00545415))

tree_summary2020$year = "pre"
tree_summary2022$year = "post"

tree_summary2020 = subset(tree_summary2020, Trt!="Control")
tree_summary2022 = subset(tree_summary2022, Trt!="Ctrl")

tree_summary = rbind(tree_summary2020, tree_summary2022)

png(file="LonePine_Figs/BA_comparison.png", width=2000, height=1600, res=200)
ggplot(tree_summary, aes(x=BA, fill=year, color=year))+
  geom_histogram(alpha=0.4, position="identity", binwidth = 20)+
  scale_fill_manual(values = c("blue", "gold"))+
  scale_color_manual(values = c("darkblue", "darkorange"))+
  labs(x="BA/ac", y="Number of Plots")+
  theme_light()+
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 16),
    plot.title = element_text(size = 22, hjust = 0.5))
dev.off()

#Canopy####

canopy2020 = read_csv("Lone_Pine_Canopy.csv")
overstory2020 = subset(overstory2020, Plot>=25, Plot<=66)
overstory2020 = subset(overstory2020, Block!="Brenna_thesis")

canopy2022 = read_csv("LonePine2022_canopy.csv")
canopy2022 = merge(treatment, canopy2022, "Plot")

##Percent Canopy####

canopy_summary2022 = canopy2022 %>%
  group_by(Plot, Trt, ) %>%
  summarise(Hits = n(),
            Per_Cover = n()*100/30)

plot_summary2022 = canopy2022 %>%
  group_by(Plot, Trt, Species) %>%
  summarise(Hits = sum(hits),
            Per_Cover = n()*100/30)

#Shrubs####
shrub_pal = c("ACGL" = "red3","AMAL2" = "orange2","PRVI" = "yellow3", "QUGA" ='green3',"ROWO" = 'blue3',"SYRO" = 'purple3')

shrubs2020 = read_csv("LonePine2020_Shrubs.csv")
shrubs2020 = merge(treatment, shrubs2020, "Plot")
overstory2020 = subset(overstory2020, Plot>=25)
overstory2020 = subset(overstory2020, Plot<=66)
overstory2020 = subset(overstory2020, Block!="Brenna_thesis")

shrubs2022 = read_csv("LonePine2022_shrubs.csv")
shrubs2022 = merge(treatment, shrubs2022, "Plot")

##Percent Cover####

shrubsum2020 = shrubs2020 %>%
  group_by(Plot, Trt, Species, HeightClass) %>%
  summarise(Length_m = sum(Length_m))
shrubsum2020 = shrubsum2020 %>%
  group_by(Plot, Trt, Species, HeightClass) %>%
  summarise(Per_Cover = mean(Length_m)/30*100)

shrubsum2022 = shrubs2022 %>%
  group_by(Plot, Trt, Species, Above_BH) %>%
  summarise(Length_m = sum(Length_m))
shrubsum2022 = shrubsum2022 %>%
  group_by(Plot, Trt, Species, Above_BH) %>%
  summarise(Per_Cover = mean(Length_m)/30*100)


shrubsumA2020 = subset(shrubsum2020, HeightClass!="b")
shrubsumA2022 = subset(shrubsum2022, Above_BH="Yes")
shrubsumB2020 = subset(shrubsum2020, HeightClass!="a")
shrubsumB2022 = subset(shrubsum2022, Above_BH!="Yes")

A20=ggplot(shrubsumA2020)+
  geom_col(aes(x=Trt, y=Per_Cover, fill=Species))+
  coord_cartesian(ylim=c(0,60))+
  scale_fill_manual(values = shrub_pal)+
  labs(title="2020")

A22=ggplot(shrubsumA2022)+
  geom_col(aes(x=Trt, y=Per_Cover, fill=Species))+
  coord_cartesian(ylim=c(0,60))+
  scale_fill_manual(values = shrub_pal)+
  labs(title="2022")

B20=ggplot(shrubsumB2020)+
  geom_col(aes(x=Trt, y=Per_Cover, fill=Species))+
  coord_cartesian(ylim=c(0,60))+
  scale_fill_manual(values = shrub_pal)+
  labs(title="2020")

B22=ggplot(shrubsumB2022)+
  geom_col(aes(x=Trt, y=Per_Cover, fill=Species))+
  coord_cartesian(ylim=c(0,60))+
  scale_fill_manual(values = shrub_pal)+
  labs(title="2022")

png(file="LonePine_Figs/Shrub_Cover.png", width=1000, height=800, res=100)
grid.arrange(A20, B20, A22, B22, nrow=2)
dev.off()
  
#Regeneration####

regeneration2022 = read_csv("LonePine2022_Regen.csv")
regeneration2022 = merge(treatment, regeneration2022, "Plot")
regeneration2022 = subset(regeneration2022, Species!="SYLA")
regeneration2022$Density = as.numeric(regeneration2022$Density)

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


#Quadrats####

quadrats2020 = read_csv("LonePine2020_quadrats.csv")
quadrats2020 = subset(quadrats2020, Plot>=25)
quadrats2020 = subset(quadrats2020, Plot<=66)
quadrats2020 = subset(quadrats2020, Block!="brenna_project")
quadrats2020 = subset(quadrats2020, Block!="Bg")
quadrats2020 = subset(quadrats2020, Block!="BG")


quadrats2020 = merge(treatment, quadrats2020, "Plot")
quadrats2020[is.na(quadrats2020)] = 0

quadrats2022 = read_csv("LonePine2022_quadrats.csv")
quadrats2022 = merge(treatment, quadrats2022, "Plot")
quadrats2022[is.na(quadrats2022)] = 0

##Fuels####
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
  coord_cartesian(ylim=c(0,7))+
  scale_fill_manual(values=fuel_palette, labels=c("1 hr", "10 hr", "100 hr"))+
  labs(title="2020")

Fuels_2022 = ggplot(Fuel_2022, aes(fill=SizeClass, y=Tons_ac, x=Trt)) + 
  geom_col()+
  coord_cartesian(ylim=c(0,7))+
  scale_fill_manual(values=fuel_palette, labels=c("1 hr", "10 hr", "100 hr"))+
  labs(title="2022")
  


png(file="LonePine_Figs/Fuels.png", width=1000, height=800, res=100)
grid.arrange(Fuels_2020, Fuels_2022, ncol=2, layout_matrix = rbind(c(1,1,1,1,2,2,2,2,2),
                                                                     c(1,1,1,1,2,2,2,2,2),
                                                                     c(1,1,1,1,2,2,2,2,2)))
dev.off()



fuel_summary2020 = quadrats2020 %>%
  group_by(Plot, Trt) %>%
  summarise(FuelLoading = mean(Total))

fuel_summary2022 = quadrats2022 %>%
  group_by(Plot, Trt) %>%
  summarise(FuelLoading = mean(Total2))


treatment_summary = plot_summary %>%
  group_by(Trt) %>%
  summarise(FuelLoading = mean(FuelLoading))

##Ground Cover####

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


Ground_2020 = ggplot(GroundCover_2020, aes(fill=CoverType, y=Per_Cover, x=Trt)) + 
  geom_col(show.legend=F)+
  coord_cartesian(ylim=c(0,100))
Ground_2022 = ggplot(GroundCover_2022, aes(fill=CoverType, y=Per_Cover, x=Trt)) + 
  geom_col()+
  coord_cartesian(ylim=c(0,100))

png(file="LonePine_Figs/GroundCover.png", width=1000, height=600, res=100)
grid.arrange(Ground_2020, Ground_2022, ncol=2, layout_matrix = rbind(c(1,1,1,1,2,2,2,2,2),
                                                                     c(1,1,1,1,2,2,2,2,2),
                                                                    c(1,1,1,1,2,2,2,2,2)))
dev.off()

##Functional Groups####

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
  coord_cartesian(ylim=c(0,50))
FG_2022 = ggplot(Funct_2022, aes(fill=FunctionalGroup, y=Per_Cover, x=Trt)) + 
  geom_col()+
  scale_fill_manual(values=FGColors)+
  coord_cartesian(ylim=c(0,50))


png(file="LonePine_Figs/FunctionalGroup.png", width=1200, height=600, res=100)
grid.arrange(FG_2020, FG_2022, ncol=2)
dev.off()
#All Together Now!####

##Merge Datasets####

BEST_2020 = merge(tree_summary2020, fuel_summary2020, "Plot")

BEST_2022 = merge(tree_summary2022, fuel_summary2022, "Plot")



##Summarise by Treatment####

treatment_summary2020 = BEST_2020 %>%
  group_by(Trt) %>%
  summarise(TPA = mean(TPA), 
            BA = mean(BA),
            QMD = mean(QMD),
            FuelLoading = mean(FuelLoading))

treatment_summary2022 = BEST_2022 %>%
  group_by(Trt.y) %>%
  summarise(TPA = mean(TPA), 
            BA = mean(BA),
            QMD = mean(QMD),
            FuelLoading = mean(FuelLoading))

##Export Summary Tables####

write.csv(BEST_2020,"C:/Users/ldh19/OneDrive/Desktop/LonePine\\BEST_PlotSum_2020.csv", row.names = FALSE)
write.csv(BEST_2022,"C:/Users/ldh19/OneDrive/Desktop/LonePine\\BEST_PlotSum_2022.csv", row.names = FALSE)
write.csv(treatment_summary2020,"C:/Users/ldh19/OneDrive/Desktop/LonePine\\BEST_TreatSum_2020.csv", row.names = FALSE)
write.csv(treatment_summary2022,"C:/Users/ldh19/OneDrive/Desktop/LonePine\\BEST_TreatSum_2022.csv", row.names = FALSE)



