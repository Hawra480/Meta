# Libraries
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(plotly)
library(d3heatmap)
library(heatmaply)
library(ggplot2)
library("ggdendro")
library("reshape2")
library("grid")

library(RColorBrewer)
if (!requireNamespace("BiocManager"))
  install.packages("BiocManager")
BiocManager::install("d3heatmap")
library("mutate") 

library("ggpubr") 
#-------------------------#
# 1. final heatmap 
#host, parasit, incidence
#-------------------------#
setwd("/Users/hawraal-ghafli/Desktop/Meta/heatmaps")


data1c<-read.csv("Book3.1H1.csv", na.strings=c("", NA))
dd<- cbind(data1c$Tryp_species1, data1c$host, data1c$all_prevelence_)
dev.new(width=200, height=400)
mine.heatmap <- ggplot(data = data1c, margin(1,1,5,1),wedith,  cex=.1, mapping = aes(x = host,
                                                                                     y = Tryp_species,
                                                                                     fill = all_prevelence_)) +
  geom_tile() +
  xlab(label = "") +
  facet_grid(switch = "x", scales = "free_x", space = "free_x") +
  scale_fill_gradient(name = "",
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
mine.heatmap + theme(axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust=1)) + theme(panel.background = element_blank())

###########

data1c<-read.csv("Book3.1H1.csv", na.strings=c("", NA))
dd<- data1c[which(data1c$host=="bees"),]
dd<- dd[which(dd$genus1=="Bombus_spp."),]

dev.new(width=800, height=1000)
mine.heatmap <- ggplot(data = dd, margin(1,1,7,1),wedith,  cex=.4, mapping = aes(x =Tryp_species12 ,
                                                                                     y = host_species12,
                                                                                     fill = all_prevelence_)) +
  geom_tile() +
  xlab(label = "") +
  facet_grid(switch = "x", scales = "free_x", space = "free_x") +
  scale_fill_gradient(name = "",
                      low = "#F8FF08",
                      high = "#EF1414")
theme_bw() +
  
  theme(strip.placement = "outside", # Move depth boxes to bottom of plot
        plot.title = element_text(hjust = 0.5), # Center-justify plot title
        axis.title.y = element_blank(), # Remove y-axis title
        strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF")) +
  ggtitle(label = "Microbe Class Abundance") +
  scale_y_discrete(limits = rev(levels(as.factor(mine.long$Class))))


mine.heatmap+ggpubr::rotate_x_text()+ facet_grid(~genus1, labeller=label_both, space= 'free') 
mine.heatmap + facet_grid(~genus1, scales='free') +theme(axis.text.x = element_text(size = 7, angle = 0, vjust = 1, hjust=1)) + theme(panel.background = element_blank())



#################

#-------------------------#
# good overall results
#-------------------------#


#-------------------------#
# using meta-regression models results per systems
#however we kept the avarage for this heatmap to avoid inaccurate results for  some parasite-host systems with low abundance of prevalence data (k=1). 
#-------------------------#

poolled<-read.csv("pooled.matrix_PFT.csv", na.strings=c("", NA))
#rnames <- data1bees[,1]  
poolled[is.na(poolled)] <- 0
rnames<- poolled$Row.Labels
colnames<- colnames(poolled)
#c=colnames[-1]
rownames(poolled)<-rnames



mat.melted <- melt(poolled)
nmat.melted<-mat.melted[which(mat.melted$value!="0"),] 
min(nmat.melted$value)
max(nmat.melted$value)
mine.heatmap2<- ggplot(nmat.melted, margin(1,1,5,1), wedith,  cex=.1, mapping = aes(x = Row.Labels,
                                                                                    y = variable,
                                                                                    fill = value)) +
  geom_tile() +
  facet_grid(switch = "x", scales = "free_x", space = "free_x") +
  scale_fill_gradient(name = "value",
                      low = "#FFFF00",
                      high = "#EF1414")
theme_bw() + 
  theme(strip.placement = "outside", # Move depth boxes to bottom of plot
        plot.title = element_text(hjust = 0.5), # Center-justify plot title
        strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF")) +
  ggtitle(label = "") +
  scale_y_discrete(limits = rev(levels(as.factor(mine.long$Class))))

#F8FF09

mine.heatmap2+ggpubr::rotate_x_text()
 mine.heatmap2 +theme(panel.background = element_blank())+ theme(axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5, hjust=1)) 


 ##################################

######################################
dev.new(width=200, height=400)


hostt<-read.csv("host_sum_PFT.csv", na.strings=c("", NA))

#data1c$pctInf = hostt$infected/(data1c$infected+data1c$not_infected)
p1<- hostt %>%
  mutate(host = fct_reorder(host, desc(pred))) %>%
  ggplot(hostt, aes(host, pred)) +
  geom_bar(stat='identity', fill='firebrick') +
  theme_minimal() + 
  ggtitle('') +
  ylab('') +
  xlab('') +
  geom_errorbar(aes(ymin=pred+se, ymax=pred-se), width=.2,
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 90, size= 10
                                   , vjust = .51, hjust=1))
#add horizental line 
p1 + geom_hline(aes(yintercept=0.1289895), colour="#000000", linetype="dashed")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#
hostt<-read.csv("host_sum_PFT.csv", na.strings=c("", NA))
hostt<- as.data.frame(hostt)
p2<- hostt %>%
  mutate(host = fct_reorder(host, desc(k)))%>%
  ggplot(aes(host, k)) +
  geom_bar(stat='identity', fill='slateblue') +
  theme_minimal() + 
  ggtitle('')+
  ylab('') +
  xlab('') +
  theme(axis.text.x = element_text(angle = 90, size= 10, vjust = .51, hjust=1))
p2 

p3<- hostt %>%
  mutate(host = fct_reorder(host, desc(k)))%>%
  ggplot(aes(host, hosttSL)) +
  geom_bar(stat='identity', fill='tomato3') +
  theme_minimal() + 
  ggtitle('')+
  ylab('') +
  xlab('') #+
  #theme(axis.text.x = element_text(angle = 90, size= 14, vjust = .51, hjust=0))
#add horizental line 
p3 
dev.new(width=200, height=400)
#dev.off()
#library(tidyverse)
par(mfrow=c(2,1))
pp2<- (p2 +  geom_label( aes(x=4.5, y=0.25, label="number of studies"), color="#404080"))
pp3<- (p3+scale_y_reverse()+geom_label( aes(x=4.5, y=0.25, label="sample_size"), color="#404080") )

grid.arrange(pp2, pp3, nrow=2, ncol=1, padding=unit(1, "line"))

#par(mar=c(0,5,3,3))
par(mar=c(0,5,3,3))
rint(pp2, position = c(0, 0, 0.5, 1), more = TRUE)
print(pp3, position = c(0.5, 0, 1, 1), more = TRUE)

data1c<-read.csv("Book3.1.csv", na.strings=c("", NA))

hist( x= data1c$year, main="" , ylab="count", xlab="", col= 'firebrick')


plot(hostt$total, hostt$k, main="Scatterplot Example", 
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)

############################
hostt<-read.csv("host_sum_PFT.csv", na.strings=c("", NA))

#data1c$pctInf = hostt$infected/(data1c$infected+data1c$not_infected)
p111<- hostt %>%
  mutate (host = fct_reorder(host, desc(pred)))%>%
  ggplot( aes(host, pred)) +
  geom_bar(stat='identity', fill='firebrick') +
  theme_minimal() + 
  ggtitle('Pooled estimate for different host groups')+
  ylab('') +
  xlab('') +
  geom_errorbar(aes(ymin=pred+se, ymax=pred-se), width=.2,
                position=position_dodge(.9)) +
  theme(axis.text.x = element_text(angle = 90, vjust = .51, hjust=1))
#add horizental line 
p1 + geom_hline(aes(yintercept=0.1289895), colour="#000000", linetype="dashed")



install.packages("ggplot2")
install.packages("plotly")
install.packages("heatmaply")

library(heatmaply)

