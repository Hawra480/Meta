

#-------------------------#
# 1. preping data 
#host, parasit, incidence
#-------------------------#
install.packages("heatmaply")# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(plotly)
library(d3heatmap)
library(heatmaply)
library("ggplot2")
library("ggdendro")
library("reshape2")
library("grid")
install.packages("classInt")
library("rworldmap")
library ("classInt")
library(RColorBrewer)

#-------------------------#
# 1. final heatmap 
#host, parasite, incidence
#-------------------------#
#seeing all countries X parasite (not used just for general vis)
setwd("/Users/hawraal-ghafli/Desktop/Meta/Global_prevelence")

#view in general how parasite prev vaires among countries with a heatmap.
data1c1<-read.csv("Book3.1C.csv", na.strings=c("", NA))

dev.new(width=200, height=400)
mine.heatmap1 <- ggplot(data = data1c1, margin(1,1,5,1),wedith,  cex=.1, mapping = aes(x = country,
                                                                                       y = Tryp_species1,
                                                                                       fill = all_prevelence_)) +
  geom_tile() +
  xlab(label = "") +
  facet_grid( switch = "x", scales = "free_x", space = "free_x") +
  scale_fill_gradient(name = "all_prevelence_",
                      low = "#F8FF08",
                      high = "#EF1414")
theme_bw() +
  
  theme(strip.placement = "outside", # Move depth boxes to bottom of plot
        plot.title = element_text(hjust = 0.5), # Center-justify plot title
        axis.title.y = element_blank(), # Remove y-axis title
        strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF")) +
  ggtitle(label = "") +
  scale_y_discrete(limits = rev(levels(as.factor(mine.long$Class))))


mine.heatmap+ggpubr::rotate_x_text()
mine.heatmap1 + theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust=1)) +theme(panel.background = element_blank())


#-------------------------#
# 1. #visualizing parasites for Bombus_spp 
#-------------------------#
data1c<-read.csv("Book3.1C.csv", na.strings=c("", NA))
data1c1<-data1c[which(data1c$host=="bees"),]

data1c1<-data1c[which(data1c$bees_type=="Bombus_spp. "),]

dev.new(width=200, height=400)
mine.heatmap <- ggplot(data = data1c1, margin(1,1,5,1),wedith,  cex=.19, mapping = aes(x =Tryp_species1 ,
                                                                                     y = host_species1,
                                                                                     fill = all_prevelence_)) +
  geom_tile() +
  xlab(label = "") +
  facet_grid(switch = "x", scales = "free_x", space = "free_x") +
  scale_fill_gradient(name = "all_prevelence_",
                      low = "#F8FF08",
                      high = "#EF1414")
theme_bw() +
  theme(strip.placement = "outside", # Move depth boxes to bottom of plot
        plot.title = element_text(hjust = 0.5), # Center-justify plot title
        axis.title.y = element_blank(), # Remove y-axis title
        strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF")) +
  ggtitle(label = "Microbe Class Abundance") +
  scale_y_discrete(limits = rev(levels(as.factor(mine.long$Class))))


mine.heatmap+ggpubr::rotate_x_text()
mine.heatmap + theme(axis.text.x = element_text(size = 7, angle = 45, vjust = 1, hjust=1)) + theme(panel.background = element_blank())


#-------------------------#
# 1. visualising honeybee parasites (not used in the paper just for refrence) 
#-------------------------#

data1c<-read.csv("Book3.1C.csv", na.strings=c("", NA))
data1c1<-data1c[which(data1c$host=="bees"),]

data1c1<-data1c[which(data1c$bees_type=="honey_bees"),]

dev.new(width=200, height=400)
mine.heatmap <- ggplot(data = data1c1, margin(1,1,5,1),wedith,  cex=.19, mapping = aes(x =Tryp_species1 ,
                                                                                       y = host_species1,
                                                                                       fill = all_prevelence_)) +
  geom_tile() +
  xlab(label = "") +
  facet_grid(switch = "x", scales = "free_x", space = "free_x") +
  scale_fill_gradient(name = "all_prevelence_",
                      low = "#F8FF08",
                      high = "#EF1414")
theme_bw() +
  theme(strip.placement = "outside", # Move depth boxes to bottom of plot
        plot.title = element_text(hjust = 0.5), # Center-justify plot title
        axis.title.y = element_blank(), # Remove y-axis title
        strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF")) +
  ggtitle(label = "Microbe Class Abundance") +
  scale_y_discrete(limits = rev(levels(as.factor(mine.long$Class))))


mine.heatmap+ggpubr::rotate_x_text()
mine.heatmap + theme(axis.text.x = element_text(size = 7, angle = 45, vjust = 1, hjust=1)) #+ theme(panel.background = element_blank())

#-------------------------#
# good overall results
#-------------------------#

#heatmap for all parasite-host systems
#Though meta-regression model was also used to generate these prevalence data to aviod inaccurices we used avarrage for heat map
# this is to avoid inaccuracies where there is one prevalence data per system. 
#----------------------#


data1c<-read.csv("Book3.1H1.csv", na.strings=c("", NA))
dd<- cbind(data1c$Tryp_species1, data1c$host, data1c$all_prevelence_)
dev.new(width=200, height=400)
mine.heatmap <- ggplot(data = data1c, margin(1,1,5,1),wedith,  cex=.1, mapping = aes(x = host,
                                                                                     y = Tryp_species12,
                                                                                     fill = all_prevelence_)) +
  geom_tile() +
  xlab(label = "") +
  facet_grid(switch = "x", scales = "free_x", space = "free_x") +
  scale_fill_gradient(name = "all_prevelence_",
                      low = "#F8FF08",
                      high = "#EF1414")
theme_bw() +
  
  theme(strip.placement = "outside", # Move depth boxes to bottom of plot
        plot.title = element_text(hjust = 0.5), # Center-justify plot title
        axis.title.y = element_blank(), # Remove y-axis title
        strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF")) +
  ggtitle(label = "Microbe Class Abundance") +
  scale_y_discrete(limits = rev(levels(as.factor(mine.long$Class))))


mine.heatmap+ggpubr::rotate_x_text()
mine.heatmap + theme(axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5, hjust=1)) + theme(panel.background = element_blank())


###################

dev.new(width=200, height=400)

selected<- c("1", "2", "3", "7")
data.h<- read.csv("Book3.1C.csv")
m_data<-data.h[which(data.h$all_type=="3"),]

bees_data<-data.h[which(data.h$host=="bees"),]
cruzi<-data.h[which(data.h$Tryp_species1=="Trypanosoma_cruzi"),]
lieshmania<-data.h[which(data.h$all_type=="1"),]
surra<- data.h[which(data.h$host=="camels"),]
brucie<- data.h[which(data.h$Tryp_species1=="Trypanosoma_brucei"),]

trypanosoma<- data.h[which(data.h$genus=="1"),]
noninsec_data<-data.h[which(data.h$insec==selected[2]),]
insec_data<-data.h[which(data.h$insec==selected[1]),]
theCountries <- m_data$IS03
# These are the ISO3 names of the countries you'd like to plot in red
#data.h
malDF <- data.frame(country = theCountries,
                    Global_prevelence_of_monoxenous_spp= m_data$all_prevelence)
# malDF is a data.frame with the ISO3 country names plus a variable to
# merge to the map data
prev<-c(0:100)
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")
classInt <- classInt::classIntervals(prev, n=5, style="jenks")
catMethod = classInt[["brks"]]
#getting a colour scheme from the RColorBrewer package
brewerList <- c("Greens","Greys","Oranges","OrRd"
                + ,"PuBuGn","Purples","YlGn","YlGnBu","YlOrBr","YlOrRd")
colourPalette <- RColorBrewer::brewer.pal(5,'YlOrRd')
#calling mapCountryData with the parameters from classInt and RColorBrewer
# This will join your malDF data.frame to the country map data
par(mai=c(0.6,0.1,0.4,0.1),xaxs="i",yaxs="i")

mapParams<- mapCountryData(malMap, addLegend = TRUE, nameColumnToPlot="Global_prevelence_of_monoxenous_spp",
                           missingCountryCol = gray(.7), catMethod=catMethod, colourPalette=colourPalette, borderCol='black', oceanCol='lightblue')
do.call( addMapLegend, c( mapParams
                          legendWidth=1))

###############################3
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")

theCountries <- lieshmania$IS03
dev.new(width=200, height=400)

malDF <- data.frame(country = theCountries,
                    Global_prevelence_of_Leishmania_spp.= lieshmania$all_prevelence)
prev<-c(0:100)
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")
classInt <- classInt::classIntervals(prev, n=5, style="jenks")
catMethod = classInt[["brks"]]
brewerList <- c("Greens","Greys","Oranges","OrRd", "PuBuGn","Purples","YlGn","YlGnBu","YlOrBr","YlOrRd")
colourPalette <- RColorBrewer::brewer.pal(5,'YlOrRd')
par(mai=c(0.6,0.1,0.4,0.1),xaxs="i",yaxs="i")

mapParams<- mapCountryData(malMap, addLegend = TRUE, nameColumnToPlot="Global_prevelence_of_Leishmania_spp.",
                           missingCountryCol = gray(.7), catMethod=catMethod, colourPalette=colourPalette, borderCol='black', oceanCol='lightblue')
do.call( addMapLegend, c( mapParams
                          legendWidth=1))
################################
cruzi<-data.h[which(data.h$Tryp_species1=="Trypanosoma_cruzi"),]
cruzi_insec<-cruzi[which(cruzi$insec=="1"),]

dev.new(width=200, height=400)
theCountries <- cruzi$IS03
malDF <- data.frame(country = theCountries,
                    Trypanosoma_cruzi= cruzi$all_prevelence)
prev<-c(0:100)
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")
classInt <- classInt::classIntervals(prev, n=5, style="jenks")
catMethod = classInt[["brks"]]
brewerList <- c("Greens","Greys","Oranges","OrRd"
                + ,"PuBuGn","Purples","YlGn","YlGnBu","YlOrBr","YlOrRd")
colourPalette <- RColorBrewer::brewer.pal(5,'YlOrRd')
par(mai=c(0.6,0.1,0.4,0.1),xaxs="i",yaxs="i")

mapParams<- mapCountryData(malMap, addLegend = TRUE, nameColumnToPlot="Trypanosoma_cruzi",
                           missingCountryCol = gray(.7), catMethod=catMethod, colourPalette=colourPalette,borderCol='black', oceanCol='lightblue')
do.call( addMapLegend, c( mapParams
                          legendWidth=1))
########$$$$$$$$$##################3

cruzi<-data.h[which(data.h$Tryp_species1=="Trypanosoma_cruzi"),]
cruzi_none_insec<-cruzi[which(cruzi$insec=="2"),]

dev.new(width=200, height=400)
theCountries <- cruzi$IS03
malDF <- data.frame(country = theCountries,
                    Trypanosoma_cruzi= cruzi$all_prevelence)
prev<-c(0:100)
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")
classInt <- classInt::classIntervals(prev, n=5, style="jenks")
catMethod = classInt[["brks"]]
brewerList <- c("Greens","Greys","Oranges","OrRd"
                + ,"PuBuGn","Purples","YlGn","YlGnBu","YlOrBr","YlOrRd")
colourPalette <- RColorBrewer::brewer.pal(5,'YlOrRd')
par(mai=c(0.6,0.1,0.4,0.1),xaxs="i",yaxs="i")

mapParams<- mapCountryData(malMap, addLegend = TRUE, nameColumnToPlot="Trypanosoma_cruzi",
                           missingCountryCol = gray(.7), catMethod=catMethod, colourPalette=colourPalette,borderCol='black', oceanCol='lightblue')
do.call( addMapLegend, c( mapParams
                          legendWidth=1))
########$$$$$$$$
dev.new(width=200, height=400)
data.h<- read.csv("Book3.1C.csv")

lieshmania<-data.h[which(data.h$all_type=="2"),]
lieshmania_major<-data.h[which(data.h$Tryp_species1=="Trypanosoma_dionisii"),]

lieshmania_insec<-lieshmania[which(lieshmania$insec=="1"),]
lieshmania_none_insec<-lieshmania[which(lieshmania$insec=="2"),]

theCountries <- lieshmania_major$IS03
malDF <- data.frame(country = theCountries,
                    Global_prevelence_of_lieshmania_major= lieshmania_major$all_prevelence)
prev<-c(0:100)
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")
classInt <- classInt::classIntervals(prev, n=5, style="jenks")
catMethod = classInt[["brks"]]
brewerList <- c("Greens","Greys","Oranges","OrRd"
                + ,"PuBuGn","Purples","YlGn","YlGnBu","YlOrBr","YlOrRd")
colourPalette <- RColorBrewer::brewer.pal(5,'YlOrRd')
par(mai=c(0.6,0.1,0.4,0.1),xaxs="i",yaxs="i")

mapParams<- mapCountryData(malMap, addLegend = TRUE, nameColumnToPlot="Global_prevelence_of_lieshmania_major",
                           missingCountryCol = gray(.7), catMethod=catMethod, colourPalette=colourPalette,  borderCol='black', oceanCol='lightblue')
do.call( addMapLegend, c( mapParams,
                          legendWidth=1))

######################################3
dev.new(width=200, height=400)
theCountries <- brucie$IS03
malDF <- data.frame(country = theCountries,
                    Trypanosoma_brucie_spp.= brucie$all_prevelence)
prev<-c(0:100)
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")
classInt <- classInt::classIntervals(prev, n=5, style="jenks")
catMethod = classInt[["brks"]]
brewerList <- c("Greens","Greys","Oranges","OrRd"
                + ,"PuBuGn","Purples","YlGn","YlGnBu","YlOrBr","YlOrRd")
colourPalette <- RColorBrewer::brewer.pal(5,'YlOrRd')
mapParams<- mapCountryData(malMap, addLegend = TRUE, nameColumnToPlot="Trypanosoma_brucie_spp.",
                           missingCountryCol = gray(.7), catMethod=catMethod, colourPalette=colourPalette,mapRegion="africa", borderCol='black', oceanCol='lightblue')
do.call( addMapLegend, c( mapParams
                          legendWidth=1))



######################################3
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")

vivax<-data.h[which(data.h$Tryp_species1=="Trypanosoma_vivax"),]

dev.new(width=200, height=400)
theCountries <- vivax$IS03
malDF <- data.frame(country = theCountries,
                    Trypanosoma_vivax= vivax$all_prevelence)
prev<-c(0:100)
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")
classInt <- classInt::classIntervals(prev, n=5, style="jenks")
catMethod = classInt[["brks"]]
brewerList <- c("Greens","Greys","Oranges","OrRd"
                + ,"PuBuGn","Purples","YlGn","YlGnBu","YlOrBr","YlOrRd")
colourPalette <- RColorBrewer::brewer.pal(5,'YlOrRd')

par(mai=c(0.6,0.1,0.4,0.1),xaxs="i",yaxs="i")
mapParams<- mapCountryData(malMap, addLegend = TRUE, nameColumnToPlot="Trypanosoma_vivax",
                           missingCountryCol = gray(.7), catMethod=catMethod, colourPalette=colourPalette, borderCol='black', oceanCol='lightblue')
do.call( addMapLegend, c( mapParams
                          legendWidth=1))
##############################################
par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
evansi<-data.h[which(data.h$Tryp_species1=="Trypanosoma_evansi"),]

dev.new(width=200, height=400)
theCountries <- evansi$IS03
malDF <- data.frame(country = theCountries,
                    Trypanosoma_evansi= evansi$all_prevelence)
prev<-c(0:100)
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")
classInt <- classInt::classIntervals(prev, n=5, style="jenks")
catMethod = classInt[["brks"]]
brewerList <- c("Greens","Greys","Oranges","OrRd",
                 "PuBuGn","Purples","YlGn","YlGnBu","YlOrBr","YlOrRd")
colourPalette <- RColorBrewer::brewer.pal(5,'YlOrRd')
par(mai=c(0.6,0.1,0.4,0.1),xaxs="i",yaxs="i")

mapParams<- mapCountryData(malMap, addLegend = TRUE, nameColumnToPlot="Trypanosoma_evansi",
                           missingCountryCol = gray(.7), catMethod=catMethod, colourPalette=colourPalette, borderCol='black', oceanCol='lightblue')

do.call( addMapLegend, c( mapParams
                          legendWidth=1))
#####################################################

simiae<-data.h[which(data.h$Tryp_species1=="Trypanosoma_simiae"),]

dev.new(width=200, height=200)
theCountries <- simiae$IS03
malDF <- data.frame(country = theCountries,
                    Trypanosoma_simiae_spp.= simiae$all_prevelence)
prev<-c(0:100)
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")
classInt <- classInt::classIntervals(prev, n=5, style="jenks")
catMethod = classInt[["brks"]]
brewerList <- c("Greens","Greys","Oranges","OrRd"
                + ,"PuBuGn","Purples","YlGn","YlGnBu","YlOrBr","YlOrRd")
colourPalette <- RColorBrewer::brewer.pal(5,'negpos9')
mapParams<- mapCountryData(malMap, addLegend = TRUE, nameColumnToPlot="Trypanosoma_simiae_spp.",
                           missingCountryCol = gray(.7), catMethod=catMethod, colourPalette=colourPalette, mapRegion="africa",borderCol='black', oceanCol='lightblue')
do.call( addMapLegend, c( mapParams
                          legendWidth=1))

#################################
#godfreyi

godfreyi<-data.h[which(data.h$Tryp_species1=="Trypanosoma_godfreyi"),]

dev.new(width=200, height=200)
theCountries <- godfreyi$IS03
malDF <- data.frame(country = theCountries,
                    Trypanosoma_godfreyi_spp.= godfreyi$all_prevelence)
prev<-c(0:100)
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")
classInt <- classInt::classIntervals(prev, n=5, style="jenks")
catMethod = classInt[["brks"]]
brewerList <- c("Greens","Greys","Oranges","OrRd"
                + ,"PuBuGn","Purples","YlGn","YlGnBu","YlOrBr","YlOrRd")
colourPalette <- RColorBrewer::brewer.pal(5,'YlOrRd')
mapParams<- mapCountryData(malMap, addLegend = TRUE, nameColumnToPlot="Trypanosoma_godfreyi_spp.",
                           missingCountryCol = gray(.7), catMethod=catMethod, colourPalette=colourPalette, mapRegion="africa",borderCol='black', oceanCol='lightblue')
do.call( addMapLegend, c( mapParams
                          legendWidth=1))
##############################
congolensee<-data.h[which(data.h$Tryp_species1=="Trypanosoma_congolense"),]

dev.new(width=200, height=200)
theCountries <- congolensee$IS03
malDF <- data.frame(country = theCountries,
                    Trypanosoma_congolense= congolensee$all_prevelence)
prev<-c(0:100)
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")
classInt <- classInt::classIntervals(prev, n=5, style="jenks")
catMethod = classInt[["brks"]]
brewerList <- c("Greens","Greys","Oranges","OrRd"
                + ,"PuBuGn","Purples","YlGn","YlGnBu","YlOrBr","YlOrRd")
colourPalette <- RColorBrewer::brewer.pal(5,'YlOrRd')

mapParams<- mapCountryData(malMap, addLegend = TRUE, nameColumnToPlot="Trypanosoma_congolense",
                           missingCountryCol = gray(.7), catMethod=catMethod, colourPalette=colourPalette,  mapRegion="africa",borderCol='black', oceanCol='lightblue')
do.call( addMapLegend, c( mapParams
                          legendWidth=1))
####################################
data.h<- read.csv("Book3.1C.csv")

bumblebees<-data.h[which(data.h$bees_type=="Bombus_spp. "),]

dev.new(width=200, height=200)
theCountries <- bumblebees$IS03
malDF <- data.frame(country = theCountries,
                    bumblebees_parasites= bumblebees$all_prevelence)
prev<-c(0:100)
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")
classInt <- classInt::classIntervals(prev, n=5, style="jenks")
catMethod = classInt[["brks"]]
brewerList <- c("Greens","Greys","Oranges","OrRd"
                + ,"PuBuGn","Purples","YlGn","YlGnBu","YlOrBr","YlOrRd")
colourPalette <- RColorBrewer::brewer.pal(5,'YlOrRd')

mapParams<- mapCountryData(malMap, addLegend = TRUE, nameColumnToPlot="bumblebees_parasites",
                           missingCountryCol = gray(.7), catMethod=catMethod, colourPalette=colourPalette,borderCol='black', oceanCol='lightblue')
do.call( addMapLegend, c( mapParams
                          legendWidth=1))
###################################
data.h<- read.csv("Book3.1C.csv")

honeybee<-data.h[which(data.h$bees_type=="honey_bees"),]

dev.new(width=200, height=200)
theCountries <- honeybee$IS03
malDF <- data.frame(country = theCountries,
                    honeybee_parasites= honeybee$all_prevelence)
prev<-c(0:100)
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")
classInt <- classInt::classIntervals(prev, n=5, style="jenks")
catMethod = classInt[["brks"]]
brewerList <- c("Greens","Greys","Oranges","OrRd"
                + ,"PuBuGn","Purples","YlGn","YlGnBu","YlOrBr","YlOrRd")
colourPalette <- RColorBrewer::brewer.pal(5,'YlOrRd')

mapParams<- mapCountryData(malMap, addLegend = TRUE, nameColumnToPlot="honeybee_parasites",
                           missingCountryCol = gray(.7), catMethod=catMethod, colourPalette=colourPalette,borderCol='black', oceanCol='lightblue')
do.call( addMapLegend, c( mapParams
                          legendWidth=1))
####################################
data.h<- read.csv("Book3.1C.csv")

Blasto<-data.h[which(data.h$Tryp_species1=="Blastocrithidia_spp."),]

dev.new(width=200, height=200)
theCountries <- Blasto$IS03
malDF <- data.frame(country = theCountries,
                    Blastocrithidia_spp.= Blasto$all_prevelence)
prev<-c(0:100)
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")
classInt <- classInt::classIntervals(prev, n=5, style="jenks")
catMethod = classInt[["brks"]]
brewerList <- c("Greens","Greys","Oranges","OrRd"
                + ,"PuBuGn","Purples","YlGn","YlGnBu","YlOrBr","YlOrRd")
colourPalette <- RColorBrewer::brewer.pal(5,'YlOrRd')

mapParams<- mapCountryData(malMap, addLegend = TRUE, nameColumnToPlot="Blastocrithidia_spp.",
                           missingCountryCol = gray(.7), catMethod=catMethod, colourPalette=colourPalette,borderCol='black', oceanCol='lightblue')
do.call( addMapLegend, c( mapParams
                          legendWidth=1))
##########################################
data.h<- read.csv("Book3.1C.csv")

Phytomonas<-data.h[which(data.h$Tryp_species1=="Phytomonas_spp."),]

dev.new(width=200, height=200)
theCountries <- Phytomonas$IS03
malDF <- data.frame(country = theCountries,
                    Phytomonas_spp.= Phytomonas$all_prevelence)
prev<-c(0:100)
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")
classInt <- classInt::classIntervals(prev, n=5, style="jenks")
catMethod = classInt[["brks"]]
brewerList <- c("Greens","Greys","Oranges","OrRd"
                + ,"PuBuGn","Purples","YlGn","YlGnBu","YlOrBr","YlOrRd")
colourPalette <- RColorBrewer::brewer.pal(5,'YlOrRd')

mapParams<- mapCountryData(malMap, addLegend = TRUE, nameColumnToPlot="Phytomonas_spp.",
                           missingCountryCol = gray(.7), catMethod=catMethod, colourPalette=colourPalette,borderCol='black', oceanCol='lightblue')
do.call( addMapLegend, c( mapParams
                          legendWidth=1))
###########################################
data.h<- read.csv("Book3.1C.csv")

Trypanosoma_lewisi<-data.h[which(data.h$Tryp_species1=="Trypanosoma_lewisi"),]

dev.new(width=200, height=200)
theCountries <- Trypanosoma_lewisi$IS03
malDF <- data.frame(country = theCountries,
                    Trypanosoma_lewisi= Trypanosoma_lewisi$all_prevelence)
prev<-c(0:100)
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")
classInt <- classInt::classIntervals(prev, n=5, style="jenks")
catMethod = classInt[["brks"]]
brewerList <- c("Greens","Greys","Oranges","OrRd"
                + ,"PuBuGn","Purples","YlGn","YlGnBu","YlOrBr","YlOrRd")
colourPalette <- RColorBrewer::brewer.pal(5,'YlOrRd')

mapParams<- mapCountryData(malMap, addLegend = TRUE, nameColumnToPlot="Trypanosoma_lewisi",
                           missingCountryCol = gray(.7), catMethod=catMethod, colourPalette=colourPalette,borderCol='black', oceanCol='lightblue')
do.call( addMapLegend, c( mapParams
                          legendWidth=1))
###########################################
dev.new(width=200, height=400)
theCountries <- data.h$IS03
malDF <- data.frame(country = theCountries,
                    Global_prevelence_of_tryposomatids= data.h$all_prevelence)
prev<-c(0:100)
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")
classInt <- classInt::classIntervals(prev, n=5, style="jenks")
catMethod = classInt[["brks"]]
brewerList <- c("Greens","Greys","Oranges","OrRd"
                + ,"PuBuGn","Purples","YlGn","YlGnBu","YlOrBr","YlOrRd")
colourPalette <- RColorBrewer::brewer.pal(5,'YlOrRd')
par(mai=c(0.6,0.1,0.4,0.1),xaxs="i",yaxs="i")

mapParams<- mapCountryData(malMap, addLegend = TRUE, nameColumnToPlot="Global_prevelence_of_tryposomatids",
                           missingCountryCol = gray(.7), catMethod=catMethod, colourPalette=colourPalette,borderCol='black', oceanCol='lightblue')
do.call( addMapLegend, c( mapParams
                          legendWidth=1))



data1c<-read.csv("Book3.1C.csv", na.strings=c("", NA))
data1c1<-data1c[which(data1c$insec=="1"),]
dev.new(width=200, height=400)
theCountries <- data1c1$IS03
malDF <- data.frame(country = theCountries,
                    Global_prevelence_of_tryposomatids_insec= data1c1$all_prevelence)
prev<-c(0:100)
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")
classInt <- classInt::classIntervals(prev, n=5, style="jenks")
catMethod = classInt[["brks"]]
brewerList <- c("Greens","Greys","Oranges","OrRd"
                + ,"PuBuGn","Purples","YlGn","YlGnBu","YlOrBr","YlOrRd")
colourPalette <- RColorBrewer::brewer.pal(5,'YlOrRd')
par(mai=c(0.6,0.1,0.4,0.1),xaxs="i",yaxs="i")

mapParams<- mapCountryData(malMap, addLegend = TRUE, nameColumnToPlot="Global_prevelence_of_tryposomatids_insec",
                           missingCountryCol = gray(.7), catMethod=catMethod, colourPalette=colourPalette,borderCol='black', oceanCol='lightblue')
do.call( addMapLegend, c( mapParams
                          legendWidth=1))
#################################################
data1c<-read.csv("Book3.1C.csv", na.strings=c("", NA))
data1c1<-data1c[which(data1c$insec=="2"),]
dev.new(width=200, height=400)
theCountries <- data1c1$IS03
malDF <- data.frame(country = theCountries,
                    Global_prevelence_of_tryposomatids_insec= data1c1$all_prevelence)
prev<-c(0:100)
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")
classInt <- classInt::classIntervals(prev, n=5, style="jenks")
catMethod = classInt[["brks"]]
brewerList <- c("Greens","Greys","Oranges","OrRd"
                + ,"PuBuGn","Purples","YlGn","YlGnBu","YlOrBr","YlOrRd")
colourPalette <- RColorBrewer::brewer.pal(5,'YlOrRd')
par(mai=c(0.6,0.1,0.4,0.1),xaxs="i",yaxs="i")

mapParams<- mapCountryData(malMap, addLegend = TRUE, nameColumnToPlot="Global_prevelence_of_tryposomatids_insec",
                           missingCountryCol = gray(.7), catMethod=catMethod, colourPalette=colourPalette,borderCol='black', oceanCol='lightblue')
do.call( addMapLegend, c( mapParams
                          legendWidth=1))





############################################
#. heat map of pooled prev for each parasite_host systems  using results from meta-regression models
############################################3
poolled<-read.csv("pooled.matrix.csv", na.strings=c("", NA))
#rnames <- data1bees[,1]  
poolled[is.na(poolled)] <- 0
rnames<- poolled$Row.Labels
colnames<- colnames(poolled)
c=colnames[-1]

c
mine.heatmap
rownames(poolled)<- rnames(poolled)
poolled <- data.matrix(poolled  # transform column 2-5 into a matrix
                       poolled[-1,]
                       poolled[,-1]
                       prDatTall <- data.frame(tryp = rep(colnames(poolled)), each = nrow(poolled)),
                       host = rnames,
                       est = unlist(poolled))




