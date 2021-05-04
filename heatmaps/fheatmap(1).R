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


#-------------------------#
# 1. final heatmap 
#host, parasit, incidence
#-------------------------#
setwd("/Users/hawraal-ghafli/Desktop/All/metanalysis_tryps")
data1c1<-read.csv("Book3C.csv", na.strings=c("", NA))
data1c1<- as.data.frame(data1c1)

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
  ggtitle(label = "Microbe Class Abundance") +
  scale_y_discrete(limits = rev(levels(as.factor(mine.long$Class))))



mine.heatmap+ggpubr::rotate_x_text()
mine.heatmap1 + theme(axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust=1)) +theme(panel.background = element_blank())

#legend(x="bottomleft", cex=.7, legend=c("min", "ave", "max"), 
# fill=colorRampPalette(brewer.pal(8, "Reds"))(3))

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
mine.heatmap <- ggplot(data = dd, margin(1,1,7,1),wedith,  cex=.4, mapping = aes(x =Tryp_species ,
                                                                                     y = host_species2,
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





data1c<-read.csv("Book3.1.csv", na.strings=c("", NA))
dd<- data1c[which(data1c$host=="flies"),]
#dd<- dd[which(dd$genus1=="Bombus_spp."),]
#dd<- dd[which(dd$genus1=="Apis_spp."),]

dev.new(width=200, height=400)
mine.heatmap <- ggplot(data = dd, margin(1,1,5,1),wedith,  cex=.07, mapping = aes(x =all_type2 ,
                                                                                 y = host_type,
                                                                                 fill = all_prevelence_)) +
  geom_tile() +
  xlab(label = "") +
  facet_grid(switch = "x", scales = "free_x", space = "free_x") +
  scale_fill_gradient(name = "prevelence",
                      low = "#F8FF08",
                      high = "#EF1414")
theme_bw() +
  
  theme(strip.placement = "outside", # Move depth boxes to bottom of plot
        plot.title = element_text(hjust = 0.5), # Center-justify plot title
        axis.title.y = element_blank(), # Remove y-axis title
        strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF")) +
  ggtitle(label = "Microbe Class Abundance") +
  scale_y_discrete(limits = rev(levels(as.factor(mine.long$Class))))


mine.heatmap+ggpubr::rotate_x_text()+ facet_grid(~exact_host , scales='free' )
mine.heatmap + facet_grid(~insec,  scales="free" )+theme(axis.text.x = element_text(size = 6, angle = 45, vjust = 1, hjust=1)) + theme(panel.background = element_blank())









#################
data1c<-read.csv("analysis4.csv", na.strings=c("", NA))
dev.new(width=200, height=400)
mine.heatmap <- ggplot(data = data1c, margin(1,1,5,1),wedith,  cex=.1, mapping = aes(x = C.bombi,
                                                                                     y = Tryp_species1,
                                                                                     fill = pred2)) +
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

#-------------------------#
# good overall results
#-------------------------#

h1<-heatmap(matrixh, scale="column")


h2<- heatmap(matrixh, Colv = NA, Rowv = NA, scale="column")
h3<- heatmap(matrixh, scale="column", col = cm.colors(256))
h4<-heatmap(matrixh, scale="column", col = terrain.colors(256))
library(RColorBrewer)
coul <- colorRampPalette(brewer.pal(8, "PiYG"))(25)
heatmap(matrixh, scale="column", col = coul)
dev.new(width=200, height=400)

#-------------------------#
# bees only
#-------------------------#
setwd("/Users/hawraal-ghafli/metanalysis_tryps")

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


######################################
 setwd("/Users/hawraal-ghafli/metanalysis_tryps")
 
 poolled.bee<-read.csv("bee_host_parasite.csv", na.strings=c("", NA))
 #rnames <- data1bees[,1]  
 poolled.bee[is.na(poolled.bee)] <- 0
 rnames<- poolled.bee$Row.Labels
 colnames<- colnames(poolled.bee)
 #c=colnames[-1]
 rownames(poolled.bee)<-rnames
 
 
 
 mat.melted <- melt(poolled.bee)
 nmat.melted<-mat.melted[which(mat.melted$value!="0"),] 
 nmat.melted<-nmat.melted[which(nmat.melted$value!="0.000000000"),] 
 nmat.melted<-nmat.melted[which(nmat.melted$value>="0.000000000"),] 
 
 min(nmat.melted$value)
 max(nmat.melted$value)
 mine.heatmap3<- ggplot(nmat.melted, margin(1,1,5,1), wedith,  cex=.1, mapping = aes(x = variable,
                                                                                     y = Row.Labels,
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
 
 mine.heatmap3+ggpubr::rotate_x_text()
 mine.heatmap3 +theme(panel.background = element_blank())+ theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust=1)) 
 ##################################
 hostf<-read.csv("flies_sum_av.csv", na.strings=c("", NA))
 hostf[is.na(hostf)] <- 0
 rnames<- hostf$Row.Labels
 colnames<- colnames(hostf)
 #c=colnames[-1]
 rownames(hostf)<-rnames
 
 
 
 hostf <- melt(hostf)
 hostf<-hostf[which(hostf$value!="0"),] 
 hostf<-hostf[which(hostf$value!="0.000000000"),] 
 nmat.melted<-hostf[which(hostf$value>="0.000000000"),] 
 
 min(nmat.melted$value)
 max(nmat.melted$value)
 mine.heatmap4<- ggplot(nmat.melted, margin(1,1,5,1), wedith,  cex=.1, mapping = aes(x = variable,
                                                                                     y = Row.Labels,
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
 
 mine.heatmap4+ggpubr::rotate_x_text()
 mine.heatmap4 +theme(panel.background = element_blank())+ theme(axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5, hjust=1)) 
 heatmap(d)
 
 ##################################
setwd("/Users/hawraal-ghafli/metanalysis_tryps")

poolled1<-read.csv("pooled.matrix_PFT.csv", na.strings=c("", NA))
#rnames <- data1bees[,1]  
poolled1[is.na(poolled1)] <- 0
rnames<- poolled1$Row.Labels
colnames<- colnames(poolled1)
colnames=colnames[-1]

rownames(poolled1)<-rnames

poolled1<-as.matrix(poolled1)
#rownames(poolled1)<-rnames
mat.melted1 <- melt(poolled1)
mat.melted1<- mat.melted1[-(1:19), ]
mat.melted1<-as.data.frame(mat.melted1)
mat.melted1$value<-as.factor(mat.melted1$value)
mat.melted1<-as.data.frame(mat.melted1)

nmat.melted2<-mat.melted1[which(mat.melted1$value!="NA"),] 
nmat.melted3<-mat.melted1[which(mat.melted1$value!="0.0000"),] 
nmat.melted4<-nmat.melted3[which(nmat.melted3$value!="0"),] 
nmat.melted5<-nmat.melted4[which(nmat.melted4$value!="0e+00"),] 
nmat.melted6<-nmat.melted5[which(nmat.melted5$value!="0.000"),] 
nmat.melted7<-nmat.melted6[which(nmat.melted6$value!="0.0000000"),] 
nmat.melted7<-nmat.melted7[which(nmat.melted7$value!="0.00000000"),] 
nmat.melted7<-nmat.melted7[which(nmat.melted7$value!="0.000000000"),] 

nmat.melted7<- as.data.frame(nmat.melted7)
0.000000000
#nmat.melted7$value<-as.numeric(mat.melted6$value)
#nmat.melted6$value2<-cut(nmat.melted6$value,breaks=c(-1,-0.75,-0.5,-0.25,0,0.25,0.5,0.75,1),include.lowest=TRUE, label=c("(-0.75,-1)","(-0.5,-0.75)","(-0.25,-0.5)","(0,-0.25)","(0,0.25)","(0.25,0.5)","(0.5,0.75)","(0.75,1)")) # the label for the legend

nmat.melted7$value1 <- ifelse(nmat.melted7$value== ".001", "***", ifelse(nmat.melted7$value== ".01", "** ", ifelse(nmat.melted7$value== ".05", "* ", " "))) # so 4 categories  
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
nmat.melted6$value2<-cut(as.numeric(nmat.melted6$value),breaks=c(-Inf, 0.001, 0.01, 0.05),label=c("***", "** ", "*  "))

mine.heatmap3<- ggplot(data = nmat.melted7, margin(1,1,5,1),wedith,  cex=.1, mapping = aes(x = Row.Labels,
                                                                                          y = variable,
                                                                                          fill = value)) +
  geom_tile("Risk of infection among hosts") +
  ylab(label = "Trypanosomatid_spp.") +
  xlab(label = "") +
  facet_grid(switch = "x", scales = "free_x", space = "free_x") +
  scale_fill_gradient(name = "pooled estimate",
                      low = "#F8FF08",
                      high = "#EF1414")
theme_bw() +
  
  theme(strip.placement = "outside", # Move depth boxes to bottom of plot
        plot.title = element_text(hjust = 0.5), # Center-justify plot title
        axis.title.y = element_blank(), # Remove y-axis title
        strip.background = element_rect(fill = "#EEEEEE", color = "#FFFFFF")) +
  ggtitle(label = "Microbe Class Abundance") +
  scale_y_discrete(limits = rev(levels(as.factor(mat.melted$variable))))



mine.heatmap3+ggpubr::rotate_x_text()
mine.heatmap3 +theme(panel.background = element_blank())+ theme(axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5, hjust=1)) 
heatmaply(
  nmat.melted7,
  colors = viridis(n = 256,  option = "magma"),
  k_col = 2, 
  k_row = 2
)

heatmap(nmat.melted7)
######################################
dev.new(width=200, height=400)


hostt<-read.csv("host_sum_PFT.csv", na.strings=c("", NA))

#data1c$pctInf = hostt$infected/(data1c$infected+data1c$not_infected)
p1<- hostt %>%
  mutate(host = fct_reorder(host, desc(pred)))%>%
  ggplot( aes(host, pred)) +
  geom_bar(stat='identity', fill='firebrick') +
  theme_minimal() + 
  ggtitle('')+
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
#add horizental line 
p2 

p3<- hostt %>%
  mutate(host = fct_reorder(host, desc(k)))%>%
  ggplot(aes(host, SL)) +
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
  mutate(host = fct_reorder(host, desc(pred)))%>%
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
heatmaply(scale(mat.melted6), k_row = 3, k_col = 2)

