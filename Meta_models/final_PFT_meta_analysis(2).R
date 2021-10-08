########################
#.set up 
#######################

#install.packages("devtools")
#install.packages("devtools")
#install.packages("tidyverse")
#install.packages("metafor")
#install.packages("patchwork")
#install.packages("R.rsp")
#devtools::install_github("itchyshin/orchard_plot", subdir = "orchaRd", force = TRUE, build_vignettes = TRUE)
#install.packages("Matrix")
library ("Matrix")
library ("metafor")
library ("meta")
library("dplyr")
library ("orchaRd")
library("ggplot2")
library("plyr")
library ("igraph")
library ("gridExtra")
library ("rpart")
setwd("/Users/hawraal-ghafli/Desktop/Meta/Meta_models")


#-------------------------#
# 1. factor prep for PFT.transformed data
#-------------------------#
#data1c<-read.csv("Book3.1H1.csv", na.strings=c("", NA))
data1c<-read.csv("Book3.1H2.csv", na.strings=c("", NA))

#transforming the data with PFT and preping factors 
ies.da1=escalc(xi=cases, ni=total, data=data1c, measure="PFT", add=0)
ies.da1$md<- as.factor(ies.da1$md)
ies.da1$insec<- as.factor(ies.da1$insec)
ies.da1$dm<- as.factor(ies.da1$dm)
ies.da1$year<- as.factor(ies.da1$year)
ies.da1$Tryp_species12<- as.factor(ies.da1$Tryp_species1)
ies.da1$host<- as.factor(ies.da1$host)
ies.da1$study_ID<- as.factor(ies.da1$study_ID)
ies.da1$host1<- as.factor(ies.da1$host1)
ies.da1$all_type<- as.factor(ies.da1$all_type)
ies.da1$all_type2<- as.factor(ies.da1$all_type2)
ies.da1$insec1<- as.factor(ies.da1$insec1)
ies.da1$yi<- as.numeric(ies.da1$yi)

#-------------------------#
#generic model with no_random factors
#-------------------------#
g<-rma(yi, vi,data= ies.da1, method= "REML")
g1<- rma.mv(yi, vi,data= ies.da1, mods=~md-1, method= "REML")
g2<- rma.mv(yi, vi,data= ies.da1, mods=~host-1, method= "REML")
g3<- rma.mv(yi, vi,data= ies.da1, mods=~Tryp_species1-1, method= "REML")
g4<- rma.mv(yi, vi,data= ies.da1, mods=~dm-1, method= "REML")
g5<- rma.mv(yi, vi,data= ies.da1, mods=~insec-1, method= "REML")

# back transforming model$beta values to pooled proportion   
pes.g=transf.ipft.hm(g$beta, targ=list(ni=ies.da1$total)) # 
pes.g1=transf.ipft.hm(g1$beta[1:3], targ=list(ni=ies.da1$total)) # 
pes.g2=transf.ipft.hm(g2$beta[1:19], targ=list(ni=ies.da1$total)) # 
pes.g3=transf.ipft.hm(g3$beta[1:48], targ=list(ni=ies.da1$total)) # 
pes.g4=transf.ipft.hm(g4$beta[1:3], targ=list(ni=ies.da1$total)) # 
pes.g5=transf.ipft.hm(g5$beta[1:2], targ=list(ni=ies.da1$total)) # 
pes.g6=transf.ipft.hm(g1$beta[1:3], targ=list(ni=ies.da1$total)) # 

#-------------------------#
#. model with random list (used for some meta-regressions in the study)
#-------------------------#
gr<-rma.mv(yi, vi,data= ies.da1, method= "REML",random= list(~ host|study_ID, ~Tryp_species1|study_ID) )
gr1<- rma.mv(yi, vi,data= ies.da1, mods=~md-1, method= "REML",random= list(~ host|study_ID, ~Tryp_species1|study_ID) )
gr2<- rma.mv(yi, vi,data= ies.da1, mods=~host-1, method= "REML", random= list(~ host|study_ID, ~Tryp_species1|study_ID))
gr3<- rma.mv(yi, vi,data= ies.da1, mods=~Tryp_species12-1, method= "REML", random= list(~ host|study_ID, ~Tryp_species1|study_ID))
gr4<- rma.mv(yi, vi,data= ies.da1, mods=~dm-1, method= "REML", random= list(~ host|study_ID, ~Tryp_species1|study_ID))
gr5<- rma.mv(yi, vi,data= ies.da1, mods=~insec-1, method= "REML", random= list(~ host|study_ID, ~Tryp_species1|study_ID))
gr6<- rma.mv(yi, vi,data= ies.da1, mods=~insec1-1, method= "REML", random= list(~ host|study_ID, ~Tryp_species1|study_ID))
# back transforming model$beta values to pooled proportion   

pes.gr=transf.ipft.hm(gr$beta, targ=list(ni=ies.da1$total)) # 
pes.gr1=transf.ipft.hm(gr1$beta[1:3], targ=list(ni=ies.da1$total)) # 
pes.gr2=transf.ipft.hm(gr2$beta[1:19], targ=list(ni=ies.da1$total)) # 
pes.gr3=transf.ipft.hm(gr3$beta[1:48], targ=list(ni=ies.da1$total)) # 
pes.gr4=transf.ipft.hm(gr4$beta[1:3], targ=list(ni=ies.da1$total)) # 
pes.gr5=transf.ipft.hm(gr5$beta[1:2], targ=list(ni=ies.da1$total)) # 
# running the same meta-regression models with the  exclusion of the model intercept to statiscally compare between pooled estimates of each factor/coffeccient 

gr1.1<- rma.mv(yi, vi,data= ies.da1, mods=~md, method= "REML",random= list(~ host|study_ID, ~Tryp_species12|study_ID) )
gr2.1<- rma.mv(yi, vi,data= ies.da1, mods=~host, method= "REML", random= list(~ host|study_ID, ~Tryp_species1|study_ID))
gr3.1<- rma.mv(yi, vi,data= ies.da1, mods=~Tryp_species12, method= "REML", random= list(~ host|study_ID, ~Tryp_species1|study_ID))
gr4.1<- rma.mv(yi, vi,data= ies.da1, mods=~dm, method= "REML", random= list(~ host|study_ID, ~Tryp_species12|study_ID))
gr5.1<- rma.mv(yi, vi,data= ies.da1, mods=~insec, method= "REML", random= list(~ host|study_ID, ~Tryp_species12|study_ID))
gr6<- rma.mv(yi, vi,data= ies.da1, mods=~insec1-1, method= "REML", random= list(~ host|study_ID, ~Tryp_species12|study_ID))

#-------------------------#
#.  visualization
#-------------------------#
#general view of all data values
#g<-rma(yi, vi,data= ies.da1, method= "REML")
dev.new(width=200, height=400)
model_results <- orchaRd::mod_results(gr, mod = "Int")
print(model_results)
orchard_plot(gr, mod = "Int", xlab = "Infection propotion", transfm="tanh")
caterpillars(model_results, mod = "Int", xlab = "Standardised mean difference") + 
  annotate(geom = "text", x = 1.5, y = 80, label = paste0("PFT/RE"), color = "black", parse = TRUE, size = 5) + annotate(geom = "text",x = 1.9, y = 79.5, label = "", color = "black", parse = FALSE, size = 5)


#-------------------------#
#.  visualization based on a meta-regression with parasite's life-cycle as a moderator.
#-------------------------#
dev.new(width=200, height=400)
model_results <- orchaRd::mod_results(gr1, mod = "md")
print(model_results)
orchard_plot(gr1, mod = "md", xlab = "Infection propotion among all hosts", transfm="tanh")

orchard_plot(gr5, mod = "insec", xlab = "Infection propotion among all hosts", transfm="tanh")

#-------------------------#
#.  visualization based on a meta-regression with host_Grouping (insects, non-insects) as a moderator.
#-------------------------#
xmodel_results <- orchaRd::mod_results(gr5, mod = "insec")
print(model_results)
#dev.new(width=200, height=400)
orchard_plot(gr6, angle=90, cb=TRUE, mod = "insec1", xlab = "Infection propotion among all hosts", transfm="tanh")
I2 <- i2_ml(gr6)

#-------------------------#
#.  visualization based on other meta-regressions (not used in the study).
#-------------------------#

dev.new(width=200, height=400)
h2=rma.mv(yi, vi, subset = insec=="1", mods=~all_type2-1, random= list(~ host|study_ID, ~Tryp_species12|study_ID), 
               data= ies.da1, method= "REML") 
h3=rma.mv(yi, vi, subset = insec=="2", mods=~Tryp_species12-1, random= list(~ host|study_ID, ~Tryp_species12|study_ID), 
          data= ies.da1, method= "REML") 
h4=rma.mv(yi, vi, subset = dm=="sero", random= list(~ dm|study_ID, ~Tryp_species12|study_ID), 
          data= ies.da1, method= "REML") 
h5=rma.mv(yi, vi, subset = insec=="1", mods=~md-1, random= list(~ host|study_ID, ~Tryp_species12|study_ID), 
          data= ies.da1, method= "REML") 
pes.3=transf.ipft.hm(h3$beta[1:31], targ=list(ni=ies.da1$total)) # 

orchard_plot(h5, mod = "md", xlab = "infection propotion among insects ", alpha = 0.5, 
             transfm = "tanh", angle = 0)
orchard_plot(h2, mod = "all_type2", xlab = "infection propotion Trypanosma_group ", alpha = 0.5, 
             transfm = "tanh", angle = 0)

orchard_plot(h3, mod = "all_type2", xlab = "infection propotion Trypanosma_group ", alpha = 0.5, 
             transfm = "tanh", angle = 0)
#-------------------------#
# 1. funnel plot and asymmetry for studies included qualitatively
#-------------------------#
dev.new(width=200, height=400)
#gh1<-rma(yi, vi,subset= insec=="1", data= ies.da1, method= "REML")
#gh2<- rma(yi, vi,subset= insec=="1", data= ies.da1, method= "REML")

trimfill_left <- trimfill(g, side="left") 
summary(trimfill_left) 

trimfill_right <- trimfill(g, side="right")
summary(trimfill_right)

funnel(trimfill_right, yaxis="seinv", xlab="Effect size", steps=15, digits=1, ylim=c(2, 200), xlim=c(-1, 1.5), 
       back="white", shade="white", hlines="white", pch=21, col=rgb(0,100,200, max=255), bg=rgb(255,255,255, max=255, alpha=150), cex=0.8)
abline(v=0, lty=2)
regtest(g)

#-------------------------#
# 1. meta_analysis for two groups mono, dix susbets of the data
  #1.1 to assess how different is prev of infection based on dignostic test among insects and non insects
  #1.2 to assess variable moderators among mono data 
  #1.3 to assess varaible moderators amonfg dix
#-------------------------#
# exclude mixed tryps from the data
i_data1<-data1c[which(data1c$all_type<="4"),]
#transforming the data with PFT and preping factors 
i_data1.all=escalc(xi=cases, ni=total, data=i_data1, measure="PFT", add=0) 
i_data1.all$insec<- as.factor(i_data1.all$insec)
i_data1.all$host<- as.factor(i_data1.all$host)
i_data1.all$Tryp_species1<- as.factor(i_data1.all$Tryp_species12)
i_data1.all$dm<- as.factor(i_data1.all$dm)
i_data1.all$all_type2<- as.factor(i_data1.all$all_type2)
#1.1  diagnostic method among insects and non-insects without sero
i_data12<-i_data1.all[which(i_data1.all$dm!="sero"),]
dmm1<- rma.mv(yi, vi, subset=insec==1, mods=~dm, random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= i_data12, method= "REML")
dmm2<- rma.mv(yi, vi, subset=insec==2, mods=~dm, random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= i_data12, method= "REML")
i_data12.1<-i_data12[which(i_data12$insec=="1"),]
i_data12.2<-i_data12[which(i_data12$insec=="2"),]

pes.hdmm1=transf.ipft.hm(dmm1$beta[1:2], targ=list(ni=i_data12.1$total)) # 
pes.hdmm2=transf.ipft.hm(dmm2$beta[1:2], targ=list(ni=i_data12.2$total)) # 
##################################################
#to generally view inspect how groups differ among insects 
i.4<- rma.mv(yi, vi, subset=insec==1, mods=~all_type2-1, random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= i_data1.all, method= "REML")
dev.new(width=200, height=400)
orchard_plot(i.4, mod = "all_type2", xlab = "infection propotion caused by monoxenous_genera", alpha = 0.5, 
             transfm = "tanh", angle = 0)

#1.2 to assess variable moderators among mono data 
#create a subset with only mono infections
m_data<-i_data1[which(i_data1$md=="monoxenous_spp."),]
ies.dm=escalc(xi=cases, ni=total, data=m_data, measure="PFT", add=0) 
ies.dm$insec<- as.factor(ies.dm$insec)
ies.dm$host<- as.factor(ies.dm$jost)
ies.dm$Tryp_species12<- as.factor(ies.dm$Tryp_species12)
ies.dm$dm<- as.factor(ies.dm$dm)
#create a subset with only dix infections
dix_data<-i_data1[which(i_data1$md=="dixenous_spp."),]
ies.dd=escalc(xi=cases, ni=total, data=dix_data, measure="PFT", add=0) 
ies.dd$insec<- as.factor(ies.dd$insec)
ies.dd$host<- as.factor(ies.dd$jost)
ies.dd$Tryp_species12<- as.factor(ies.dd$Tryp_species12)
ies.dd$dm<- as.factor(ies.dd$dm)
ies.dd$all_type2<- as.factor(ies.dd$all_type2)
#mono subset meta-regression models 
#general model w random factors
m<- rma.mv(yi, vi, random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= ies.dm, method= "REML")
#meta_regeressions each  with a moderator and nested random factor
m.1<- rma.mv(yi, vi,  mods=~dm-1, random= list(~ host|study_ID, ~Tryp_species|study_ID), data= ies.dm, method= "REML")
m.2<- rma.mv(yi, vi,  mods=~host-1, random= list(~ host|study_ID, ~Tryp_species|study_ID), data= ies.dm, method= "REML")
m.3<- rma.mv(yi, vi,  mods=~Tryp_species12, random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= ies.dm, method= "REML")
m.4<- rma.mv(yi, vi,  mods=~insec, random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= ies.dm, method= "REML")
m.5<- rma.mv(yi, vi,  mods=~genus-1, subset=insec=="1", random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= ies.dm, method= "REML")
#back-transforming each coffecient in a moderator to proportion values
pes.m.5=transf.ipft.hm(m.5$beta[1:9], targ=list(ni=ies.dm$total)) # 
pes.m.1=transf.ipft.hm(m.1$beta[1:2], targ=list(ni=ies.dm$total)) # 
pes.m.2=transf.ipft.hm(m.2$beta[1:5], targ=list(ni=ies.dm$total)) # 
pes.m.3=transf.ipft.hm(m.3$beta[1:12], targ=list(ni=ies.dm$total)) # 
pes.m.4=transf.ipft.hm(m.4$beta[1:2], targ=list(ni=ies.dm$total)) # 
dev.new(width=200, height=400)
#visulaisation of genus based coffecients among insects of mono infections 
orchard_plot(m.5, mod = "genus", xlab = "infection propotion caused by monoxenous_genera", alpha = 0.5, 
             transfm = "tanh", angle = 0)
# removing random factor and understanding hetrogenity 
m1.1<- rma.uni(yi, vi,  mods=~dm, random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= ies.dm, method= "REML")
m1.2<- rma.uni(yi, vi,  mods=~host, random= list(~ host|study_ID, ~Tryp_species1|study_ID), data= ies.dm, method= "REML")
m1.3<- rma.uni(yi, vi,  mods=~Tryp_species12, random= list(~ host|study_ID, ~Tryp_species1|study_ID), data= ies.dm, method= "REML")
m1.4<- rma.mv(yi, vi,  mods=~insec, random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= ies.dm, method= "REML")
m1.5<- rma.mv(yi, vi,  mods=~all_type2, random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= ies.dm, method= "REML")

pes.m1.1=transf.ipft.hm(m1.1$beta[1:2], targ=list(ni=ies.dm$total)) # 
pes.m1.2=transf.ipft.hm(m1.2$beta[1:5], targ=list(ni=ies.dm$total)) # 
pes.m1.3=transf.ipft.hm(m1.3$beta[1:12], targ=list(ni=ies.dm$total)) # 
pes.m1.4=transf.ipft.hm(m1.4$beta[1:2], targ=list(ni=ies.dm$total)) # 


#1.3 to assess variable moderators among dix data 
#general model with random factor design
d<- rma.mv(yi, vi, random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= ies.dd, method= "REML")
#meta_regeressions each  with a moderator and nested random factor (insects or non-insects)
d.1<- rma.mv(yi, vi,  mods=~all_type2, random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= ies.dd, method= "REML")
d.2<- rma.mv(yi, vi,  mods=~host-1, random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= ies.dd, method= "REML")
d.3<- rma.mv(yi, vi,  mods=~Tryp_species1-1, random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= ies.dd, method= "REML")
d.4<- rma.mv(yi, vi,  mods=~insec, random= list(~ host|study_ID, ~Tryp_species1|study_ID), data= ies.dd, method= "REML")
d.5 <- rma.mv(yi, vi, subset = insec=='1', mods=~all_type2-1, random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= ies.dd, method= "REML")
d.51 <- rma.mv(yi, vi, subset = insec=='1', mods=~all_type2, random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= ies.dd, method= "REML")
d.11<- rma.mv(yi, vi,  mods=~dm, random= list(~ host|study_ID, ~Tryp_species1|study_ID), data= ies.dd, method= "REML")
d.12<- rma.mv(yi, vi,  mods=~dm-1, subset=insec=="2", random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= ies.dd, method= "REML")
d.12<- rma.mv(yi, vi,  mods=~dm, subset=host=="Sheep", random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= ies.dd, method= "REML")
pes.d.5=transf.ipft.hm(d.11$beta[1:3], targ=list(ni=ies.dd$total)) # 
#back-transforming each coefficient in a moderator to proportion values
pes.d=transf.ipft.hm(d$beta[1], targ=list(ni=ies.dm$total)) # 
pes.d.1=transf.ipft.hm(d.1$beta[1:3], targ=list(ni=ies.dm$total)) # 
pes.d.2=transf.ipft.hm(d.2$beta[1:18], targ=list(ni=ies.dm$total)) # 
pes.d.3=transf.ipft.hm(d.3$beta[1:34], targ=list(ni=ies.dm$total)) # 
pes.d.4=transf.ipft.hm(d.4$beta[1:2], targ=list(ni=ies.dm$total)) # 
dev.new(width=200, height=400)
#visualization of genera among insects of dix infections 
orchard_plot(d.5, mod = "all_type2", xlab = "infection propotion cuased by dixenous_genera", alpha = 0.5, 
             transfm = "tanh", angle = 0)
pes.d.5=transf.ipft.hm(d.5$beta[1:3], targ=list(ni=ies.dm$total)) # 
#repeating model with model intercept to compare between coefficients statistically  
d1.1<- rma.uni(yi, vi,  mods=~dm, random= list(~ host|study_ID, ~Tryp_species1|study_ID), data= ies.dd, method= "REML")
d1.2<- rma.uni(yi, vi,  mods=~host, random= list(~ host|study_ID, ~Tryp_species1|study_ID), data= ies.dd, method= "REML")
d1.3<- rma.uni(yi, vi,  mods=~Tryp_species12, random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= ies.dd, method= "REML")
d1.4<- rma.uni(yi, vi,  mods=~insec, random= list(~ host|study_ID, ~Tryp_species1|study_ID), data= ies.dd, method= "REML")
pes.d1.1=transf.ipft.hm(d1.1$beta[1:3], targ=list(ni=ies.dm$total)) # 
pes.d1.2=transf.ipft.hm(d1.2$beta[1:17], targ=list(ni=ies.dm$total)) # 
pes.d1.3=transf.ipft.hm(d1.3$beta[1:34], targ=list(ni=ies.dm$total)) # 
pes.d1.4=transf.ipft.hm(d1.4$beta[1:2], targ=list(ni=ies.dm$total)) # 
#################################################3
#.meta-regression models amonfg insects only (dix vs mono)
###############################################3
#limiting the data for dix infections
insect_data1<-i_data1[which(i_data1$md=="dixenous_spp."),]
#within dix infections , selecting only those among insects
insect_data2<-insect_data1[which(insect_data1$insec=="1"),]
#transforming the data
insect_data_d=escalc(xi=cases, ni=total, data=insect_data2, measure="PFT", add=0) 
insect_data_d$host<-as.factor(insect_data_d$host)
insect_data_d$Tryp_species12<-as.factor(insect_data_d$Tryp_species12)
insect_data_d$md<-as.factor(insect_data_d$md)
insect_data_d$all_type2<-as.factor(insect_data_d$all_type2)
insect_data_d$dm<-as.factor(insect_data_d$dm)

#subset of mixed tryps only
insect_dattry<-data1c[which(data1c$md=="Trypanosomatidae_spp."),]

#subset of mono tryps only
insect_datt<-i_data1[which(i_data1$md=="monoxenous_spp."),]
#limiting the analysis to insects only 
insect_datam<-insect_datt[which(insect_datt$insec=="1"),]
#transforming the data and preparing the factors/coefficients
insect_datam1=escalc(xi=cases, ni=total, data=insect_datam, measure="PFT", add=0) 
insect_datam1$host<-as.factor(insect_datam1$host)
insect_datam1$Tryp_species12<-as.factor(insect_datam1$Tryp_species12)
insect_datam1$md<-as.factor(insect_datam1$md)
insect_datam1$all_type2<-as.factor(insect_datam1$all_type2)
insect_datam1$dm<-as.factor(insect_datam1$dm)
insect_datam1$genus<-as.factor(insect_datam1$genus)

#starting to analyze insects (mono)
m_insec1<- rma.mv(yi, vi,  mods=~host-1, random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= insect_datam1, method= "REML")
m_insec2<- rma.mv(yi, vi,  mods=~Tryp_species12-1, random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= insect_datam1, method= "REML")
m_insec3<- rma.mv(yi, vi,  mods=~dm-1, random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= insect_datam1, method= "REML")
m_insec4<- rma.mv(yi, vi,  mods=~dm-1, random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= insect_datam1, method= "REML")
m_insec5=rma.mv(yi,vi,data=insect_datam1,test="t", subset=dm==("mol"), random= list(~ host|study_ID, ~Tryp_species1|study_ID) )
summary (m_insec1)

pes.m_insec1=transf.ipft.hm(m_insec1$beta[1:4], targ=list(ni=insect_datam1$total)) # 
pes.m_insec2=transf.ipft.hm(m_insec1$beta[1:4], targ=list(ni=insect_datam1$total)) # 
pes.m_insec3=transf.ipft.hm(m_insec1$beta[1:4], targ=list(ni=insect_datam1$total)) # 
dev.new(width=200, height=400)

orchard_plot(m_insec1, mod = "host", xlab = "infection propotion Trypanosma_group ", alpha = 0.5, 
             transfm = "tanh", angle = 0)

#starting to analyze insects (dix)
d_insec1<- rma.mv(yi, vi,  mods=~host-1, random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= insect_data_d, method= "REML")
d_insec2<- rma.mv(yi, vi,  mods=~Tryp_species12-1, random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= insect_data_d, method= "REML")
d_insec3<- rma.mv(yi, vi,  mods=~dm-1, random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= insect_data_d, method= "REML")
pes.d_insec1=transf.ipft.hm(d_insec1$beta[1:4], targ=list(ni=insect_datam1$total)) # 
pes.d_insec2=transf.ipft.hm(d_insec2$beta[1:4], targ=list(ni=insect_datam1$total)) # 
pes.d_insec3=transf.ipft.hm(d_insec3$beta[1:3], targ=list(ni=insect_datam1$total)) # 
d=rma.mv(yi,vi,data=insect_data_d,test="t", subset=dm==("mol"), random= list(~ host|study_ID, ~Tryp_species12|study_ID) )
d_insec4<- rma.mv(yi, vi,  mods=~dm-1, random= list(~ host|study_ID, ~Tryp_species12|study_ID), data= insect_data_d, method= "REML")


#######################################3
# testing the full list along (confirmatory model) with the systematically included data
data1cH<-read.csv("Book3.1H2.csv", na.strings=c("", NA))
#transforming the data and preparing the factors/coefficients
ies.da1H=escalc(xi=cases, ni=total, data=data1cH, measure="PFT", add=0)
ies.da1H$md<- as.factor(ies.da1H$md)
ies.da1H$insec<- as.factor(ies.da1H$insec)
ies.da1H$dm<- as.factor(ies.da1H$dm)
ies.da1H$year<- as.factor(ies.da1H$year)
ies.da1H$Tryp_species12<- as.factor(ies.da1H$Tryp_species12)
ies.da1H$host<- as.factor(ies.da1H$host)
ies.da1H$study_ID<- as.factor(ies.da1H$study_ID)
ies.da1H$all_type<- as.factor(ies.da1H$all_type)
ies.da1H$all_type2<- as.factor(ies.da1H$all_type2)
ies.da1H$insec1<- as.factor(ies.da1H$insec1)
ies.da1H$md<- as.factor(ies.da1H$md)


#-------------------------#
#. model with random list  (confirmatory model)
#-------------------------#
gH<-rma(yi, vi,data= ies.da1H, method= "REML")
g1H<- rma.mv(yi, vi,data= ies.da1H, mods=~md-1, method= "REML")
g2H<- rma.mv(yi, vi,data= ies.da1H, mods=~host-1, method= "REML")
g3H<- rma.mv(yi, vi,data= ies.da1H, mods=~Tryp_species12-1, method= "REML")
g4H<- rma.mv(yi, vi,data= ies.da1H, mods=~dm-1, method= "REML")
g5H<- rma.mv(yi, vi,data= ies.da1H, mods=~insec-1, method= "REML")

# back trasforming model$beta values to pooled prevalence
pes.gH=transf.ipft.hm(gH$beta, targ=list(ni=ies.da1H$total)) # 
pes.g1H=transf.ipft.hm(g1H$beta[1:3], targ=list(ni=ies.da1H$total)) # 
pes.g2H=transf.ipft.hm(g2H$beta[1:19], targ=list(ni=ies.da1H$total)) # 
pes.g3H=transf.ipft.hm(g3H$beta[1:48], targ=list(ni=ies.da1H$total)) # 
pes.g4H=transf.ipft.hm(g4H$beta[1:3], targ=list(ni=ies.da1H$total)) # 
pes.g5H=transf.ipft.hm(g5H$beta[1:2], targ=list(ni=ies.da1H$total)) # 

grH<-rma.mv(yi, vi,data= ies.da1H, method= "REML",random= list(~ host|study_ID, ~Tryp_species12|study_ID) )
gr1H<- rma.mv(yi, vi,data= ies.da1H, mods=~md-1, method= "REML",random= list(~ host|study_ID, ~Tryp_species12|study_ID) )
gr2H<- rma.mv(yi, vi,data= ies.da1H, mods=~host-1, method= "REML", random= list(~ host|study_ID, ~Tryp_species12|study_ID))
gr3H<- rma.mv(yi, vi,data= ies.da1H, mods=~Tryp_species1-1, method= "REML", random= list(~ host|study_ID, ~Tryp_species12|study_ID))
gr4H<- rma.mv(yi, vi,data= ies.da1H, mods=~insec-1, method= "REML", random= list(~ host|study_ID, ~Tryp_species12|study_ID))
summary (gr1H)
summary (gr1)
summary (gr4H)
summary (gr4)
pes.grH=transf.ipft.hm(grH$beta, targ=list(ni=ies.da1$total)) # 
pes.gr1H=transf.ipft.hm(gr1H$beta[1:3], targ=list(ni=ies.da1$total)) # 
pes.gr2H=transf.ipft.hm(gr2H$beta[1:19], targ=list(ni=ies.da1$total)) # 
pes.gr3H=transf.ipft.hm(gr3H$beta[1:48], targ=list(ni=ies.da1$total)) # 
pes.gr4H=transf.ipft.hm(gr4H$beta[1:3], targ=list(ni=ies.da1$total)) # 

gr1.1<- rma.mv(yi, vi,data= ies.da1, mods=~md, method= "REML",random= list(~ host|study_ID, ~Tryp_species12|study_ID) )
gr2.1<- rma.mv(yi, vi,data= ies.da1, mods=~host, method= "REML", random= list(~ host|study_ID, ~Tryp_species12|study_ID))
gr3.1<- rma.mv(yi, vi,data= ies.da1, mods=~Tryp_species1, method= "REML", random= list(~ host|study_ID, ~Tryp_species12|study_ID))
gr4.1<- rma.mv(yi, vi,data= ies.da1, mods=~dm, method= "REML", random= list(~ host|study_ID, ~Tryp_species12|study_ID))

dev.new(width=200, height=400)
#gh1<-rma(yi, vi,subset= insec=="1", data= ies.da1, method= "REML")
#gh2<- rma(yi, vi,subset= insec=="1", data= ies.da1, method= "REML")
#assessing publication bais for the confirmatory model
trimfill_left <- trimfill(gH, side="left") 
summary(trimfill_left) 

trimfill_right <- trimfill(gH, side="right")
summary(trimfill_right)

funnel(trimfill_right, yaxis="seinv", xlab="Effect size", steps=15, digits=1, ylim=c(2, 200), xlim=c(-1, 1.5), 
       back="white", shade="white", hlines="white", pch=21, col=rgb(0,100,200, max=255), bg=rgb(255,255,255, max=255, alpha=150), cex=0.8)
abline(v=0, lty=2)
regtest(gH)

dev.new(width=200, height=400)
model_results <- orchaRd::mod_results(gr, mod = "Int")
print(model_results)
orchard_plot(gr, mod = "Int", xlab = "Infection propotion", transfm="tanh")
caterpillars(model_results, mod = "Int", xlab = "Standardised mean difference") + 
  annotate(geom = "text", x = 1.5, y = 80, label = paste0("PFT/RE"), color = "black", parse = TRUE, size = 5) + annotate(geom = "text",x = 1.9, y = 79.5, label = "", color = "black", parse = FALSE, size = 5)


dev.new(width=200, height=400)
model_results <- orchaRd::mod_results(gr1H, mod = "md")
print(model_results)
orchard_plot(gr1H, mod = "md", xlab = "Infection propotion among all hosts", transfm="tanh")
 

iH<-data1cH[which(data1cH$sm=="meta"),]
iH1<-data1cH[which(data1cH$sm=="sys"),]
tst <- c(unique(iH$study_ID),unique(iH1$study_ID))
tst <- tst[duplicated(tst)]
tst[duplicated(tst)]
######################################
#back to the original data -only studies included quantitatively
#meta regression models for each host level 
##################################
p1=rma.mv(yi,vi,data=ies.da1, mods=~ Tryp_species1-1, subset=host=="dogs", random= list( ~Tryp_species12|study_ID),)
p2=rma.mv(yi,vi,data=ies.da1, mods=~ Tryp_species1-1, subset=host=="camels", random= list(~Tryp_species12|study_ID), )
p3=rma.mv(yi,vi,data=ies.da1, mods=~ Tryp_species1-1, subset=host=="cats", random= list(~Tryp_species12|study_ID), )
p4=rma.mv(yi,vi,data=ies.da1, mods=~ Tryp_species1-1, subset=host=="bees", random= list(~Tryp_species12|study_ID), )
p5=rma.mv(yi,vi,data=ies.da1, mods=~ Tryp_species1-1, subset=host=="birds", random= list(~Tryp_species12|study_ID), )
p6=rma.mv(yi,vi,data=ies.da1, mods=~ Tryp_species1-1, subset=host=="animals", random= list(~Tryp_species12|study_ID), )
p7=rma.mv(yi,vi,data=ies.da1, mods=~ Tryp_species1-1, subset=host=="goats", random= list(~Tryp_species12|study_ID), )
p8=rma.mv(yi,vi,data=ies.da1, mods=~ Tryp_species1-1, subset=host=="truebugs", random= list(~Tryp_species12|study_ID), )
p9=rma.mv(yi,vi,data=ies.da1, mods=~ Tryp_species1-1, subset=host=="rodents", random= list(~Tryp_species12|study_ID), )
p10=rma.mv(yi,vi,data=ies.da1, mods=~ Tryp_species1-1, subset=host=="bats", random= list(~Tryp_species1|study_ID), )
p11=rma.mv(yi,vi,data=ies.da1, mods=~ Tryp_species1-1, subset=host=="cattle", random= list(~Tryp_species12|study_ID), )
p12=rma.mv(yi,vi,data=ies.da1, mods=~ Tryp_species1-1, subset=host=="equids", random= list(~Tryp_species12|study_ID), )
p13=rma.mv(yi,vi,data=ies.da1, mods=~ Tryp_species1-1, subset=host=="pigs", random= list(~Tryp_species12|study_ID), )
p14=rma.mv(yi,vi,data=ies.da1, mods=~ Tryp_species1-1, subset=host=="Sheep", random= list(~Tryp_species12|study_ID), )
p15=rma.mv(yi,vi,data=ies.da1, mods=~ Tryp_species1-1, subset=host=="flies", random= list(~Tryp_species12|study_ID), )
p16=rma.mv(yi,vi,data=ies.da1, mods=~ Tryp_species1-1, subset=host=="raccoons" )
p17=rma.mv(yi,vi,data=ies.da1, mods=~ Tryp_species1-1, subset=host=="buffaloes", random= list(~Tryp_species12|study_ID), )
p18=rma.mv(yi,vi,data=ies.da1, mods=~ Tryp_species1-1, subset=host=="fish", random= list(~Tryp_species12|study_ID), )
p19=rma.mv(yi,vi,data=ies.da1, mods=~ Tryp_species1-1, subset=host=="fleas", random= list(~Tryp_species12|study_ID), )

pes.p1=transf.ipft.hm(p1$beta[1:9], targ=list(ni=ies.da1$total)) 
pes.p2=transf.ipft.hm(p2$beta[1:3], targ=list(ni=ies.da1$total)) 
pes.p3=transf.ipft.hm(p3$beta[1:4], targ=list(ni=ies.da1$total)) 
pes.p4=transf.ipft.hm(p4$beta[1:7], targ=list(ni=ies.da1$total)) 
pes.p5=transf.ipft.hm(p5$beta[1:3], targ=list(ni=ies.da1$total)) 
pes.p6=transf.ipft.hm(p6$beta[1:16], targ=list(ni=ies.da1$total)) 
pes.p7=transf.ipft.hm(p7$beta[1:4], targ=list(ni=ies.da1$total)) 
pes.p8=transf.ipft.hm(p8$beta[1:8], targ=list(ni=ies.da1$total)) 
pes.p9=transf.ipft.hm(p9$beta[1:6], targ=list(ni=ies.da1$total)) 
pes.p10=transf.ipft.hm(p10$beta[1:5], targ=list(ni=ies.da1$total)) 
pes.p11=transf.ipft.hm(p11$beta[1:7], targ=list(ni=ies.da1$total)) 
pes.p12=transf.ipft.hm(p12$beta[1:7], targ=list(ni=ies.da1$total)) 
pes.p13=transf.ipft.hm(p13$beta[1:7], targ=list(ni=ies.da1$total)) 
pes.p14=transf.ipft.hm(p14$beta[1:6], targ=list(ni=ies.da1$total)) 
pes.p15=transf.ipft.hm(p15$beta[1:21], targ=list(ni=ies.da1$total)) 
pes.p16=transf.ipft.hm(p16$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.p17=transf.ipft.hm(p17$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.p18=transf.ipft.hm(p18$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.p19=transf.ipft.hm(p19$beta[1:2], targ=list(ni=ies.da1$total)) 


pes.da.subgroup1=rma.mv(yi,vi,data=ies.da1, subset=host=="dogs", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup2=rma.mv(yi,vi,data=ies.da1, subset=host=="camels", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup3=rma.mv(yi,vi,data=ies.da1, subset=host=="cats", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup4=rma.mv(yi,vi,data=ies.da1, subset=host=="bees", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup5=rma.mv(yi,vi,data=ies.da1, subset=host=="birds", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup6=rma.mv(yi,vi,data=ies.da1, subset=host=="animals", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup7=rma.mv(yi,vi,data=ies.da1, subset=host=="goats", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup8=rma.mv(yi,vi,data=ies.da1, subset=host=="truebugs", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup9=rma.mv(yi,vi,data=ies.da1, subset=host=="rodents", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup10=rma.mv(yi,vi,data=ies.da1, subset=host=="bats", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup11=rma.mv(yi,vi,data=ies.da1, subset=host=="cattle", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup12=rma.mv(yi,vi,data=ies.da1, subset=host=="equids", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup13=rma.mv(yi,vi,data=ies.da1, subset=host=="Sheep", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup14=rma.mv(yi,vi,data=ies.da1, subset=host=="flies", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup15=rma.mv(yi,vi,data=ies.da1, subset=host=="raccoons", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup16=rma.mv(yi,vi,data=ies.da1, subset=host=="buffaloes", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup17=rma.mv(yi,vi,data=ies.da1, subset=host=="fish", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup18=rma.mv(yi,vi,data=ies.da1, subset=host=="fleas", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup19=rma.mv(yi,vi,data=ies.da1, subset=host=="pigs", random= list(~ Tryp_species12|study_ID), )

transf1=transf.ipft.hm,targ=list(ni=ies.da1$total)
#

pes.subgroup1=predict(pes.da.subgroup1, transf=transf.ipft.hm,targ=list(ni=ies.da1$total))
pes.subgroup2=predict(pes.da.subgroup2, transf=transf.ipft.hm,targ=list(ni=ies.da1$total))
pes.subgroup3=predict(pes.da.subgroup3, transf=transf.ipft.hm,targ=list(ni=ies.da1$total))
pes.subgroup4=predict(pes.da.subgroup4, transf=transf.ipft.hm,targ=list(ni=ies.da1$total))
pes.subgroup5=predict(pes.da.subgroup5, transf=transf.ipft.hm,targ=list(ni=ies.da1$total))
pes.subgroup6=predict(pes.da.subgroup6, transf=transf.ipft.hm,targ=list(ni=ies.da1$total))
pes.subgroup7=predict(pes.da.subgroup7, transf=transf.ipft.hm,targ=list(ni=ies.da1$total))
pes.subgroup8=predict(pes.da.subgroup8, transf=transf.ipft.hm,targ=list(ni=ies.da1$total))
pes.subgroup9=predict(pes.da.subgroup9, transf=transf.ipft.hm,targ=list(ni=ies.da1$total))
pes.subgroup10=predict(pes.da.subgroup10, transf=transf.ipft.hm,targ=list(ni=ies.da1$total))
pes.subgroup11=predict(pes.da.subgroup11, transf=transf.ipft.hm,targ=list(ni=ies.da1$total))
pes.subgroup12=predict(pes.da.subgroup12, transf=transf.ipft.hm,targ=list(ni=ies.da1$total))
pes.subgroup13=predict(pes.da.subgroup13, transf=transf.ipft.hm,targ=list(ni=ies.da1$total))
pes.subgroup14=predict(pes.da.subgroup14, transf=transf.ipft.hm,targ=list(ni=ies.da1$total))
pes.subgroup15=predict(pes.da.subgroup15, transf=transf.ipft.hm,targ=list(ni=ies.da1$total))
pes.subgroup16=predict(pes.da.subgroup16, transf=transf.ipft.hm,targ=list(ni=ies.da1$total))
pes.subgroup17=predict(pes.da.subgroup17, transf=transf.ipft.hm,targ=list(ni=ies.da1$total))
pes.subgroup18=predict(pes.da.subgroup18, transf=transf.ipft.hm,targ=list(ni=ies.da1$total))
pes.subgroup19=predict(pes.da.subgroup19, transf=transf.ipft.hm,targ=list(ni=ies.da1$total))

pes.p1=transf.ipft.hm(pes.da.subgroup1$se, targ=list(ni=ies.da1$total)) 
pes.p2=transf.ipft.hm(pes.da.subgroup2$se, targ=list(ni=ies.da1$total)) 
pes.p3=transf.ipft.hm(pes.da.subgroup3$se, targ=list(ni=ies.da1$total)) 
pes.p4=transf.ipft.hm(pes.da.subgroup4$se, targ=list(ni=ies.da1$total)) 
pes.p5=transf.ipft.hm(pes.da.subgroup5$se, targ=list(ni=ies.da1$total)) 
pes.p6=transf.ipft.hm(pes.da.subgroup6$se, targ=list(ni=ies.da1$total)) 
pes.p7=transf.ipft.hm(pes.da.subgroup7$se, targ=list(ni=ies.da1$total)) 
pes.p8=transf.ipft.hm(pes.da.subgroup8$se, targ=list(ni=ies.da1$total)) 
pes.p9=transf.ipft.hm(pes.da.subgroup9$se, targ=list(ni=ies.da1$total)) 
pes.p10=transf.ipft.hm(pes.da.subgroup10$se, targ=list(ni=ies.da1$total)) 
pes.p11=transf.ipft.hm(pes.da.subgroup11$se, targ=list(ni=ies.da1$total)) 
pes.p12=transf.ipft.hm(pes.da.subgroup12$se, targ=list(ni=ies.da1$total)) 
pes.p13=transf.ipft.hm(pes.da.subgroup13$se, targ=list(ni=ies.da1$total)) 
pes.p14=transf.ipft.hm(pes.da.subgroup14$se, targ=list(ni=ies.da1$total)) 
pes.p15=transf.ipft.hm(pes.da.subgroup15$se, targ=list(ni=ies.da1$total)) 
pes.p16=transf.ipft.hm(pes.da.subgroup16$se, targ=list(ni=ies.da1$total)) 
pes.p17=transf.ipft.hm(pes.da.subgroup17$se, targ=list(ni=ies.da1$total)) 
pes.p18=transf.ipft.hm(pes.da.subgroup18$se, targ=list(ni=ies.da1$total)) 
pes.p19=transf.ipft.hm(pes.da.subgroup19$se, targ=list(ni=ies.da1$total)) 

# convert models to rma to get I^2 for each host group
pes.da.subgroup1=rma(yi,vi,data=ies.da1, subset=host=="dogs", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup2=rma(yi,vi,data=ies.da1, subset=host=="camels", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup3=rma(yi,vi,data=ies.da1, subset=host=="cats", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup4=rma(yi,vi,data=ies.da1, subset=host=="bees", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup5=rma(yi,vi,data=ies.da1, subset=host=="birds", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup6=rma(yi,vi,data=ies.da1, subset=host=="animals", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup7=rma(yi,vi,data=ies.da1, subset=host=="goats", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup8=rma(yi,vi,data=ies.da1, subset=host=="truebugs", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup9=rma(yi,vi,data=ies.da1, subset=host=="rodents", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup10=rma(yi,vi,data=ies.da1, subset=host=="bats", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup11=rma(yi,vi,data=ies.da1, subset=host=="cattle", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup12=rma(yi,vi,data=ies.da1, subset=host=="equids", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup13=rma(yi,vi,data=ies.da1, subset=host=="Sheep", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup14=rma(yi,vi,data=ies.da1, subset=host=="flies", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup15=rma(yi,vi,data=ies.da1, subset=host=="raccoons", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup16=rma(yi,vi,data=ies.da1, subset=host=="buffaloes", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup17=rma(yi,vi,data=ies.da1, subset=host=="fish", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup18=rma(yi,vi,data=ies.da1, subset=host=="fleas", random= list(~ Tryp_species12|study_ID), )
pes.da.subgroup19=rma(yi,vi,data=ies.da1, subset=host=="pigs", random= list(~ Tryp_species12|study_ID), )

#insects non_insects parasite comparisions 
data1c<-read.csv("Book3.1H1.csv", na.strings=c("", NA))
ies.da1=escalc(xi=cases, ni=total, data=data1c, measure="PFT", add=0)
ies.da1$genus<- as.factor(ies.da1$genus)
ies.da1$insec<- as.factor(ies.da1$insec)

ies.da2<-ies.da1[which(ies.da1$genus=="1"),]
ies.da3<-ies.da1[which(ies.da1$genus=="9"),]
ies.da3.1<-ies.da1[which(ies.da1$Tryp_species1=="Trypanosoma_cruzi"),]
ies.da4<- rbind (ies.da2, ies.da3, ies.da3.1)
head(ies.da3.1) 
#analyzing common groups of parasites with prev data among both insects and non-insects saperatly
pes.da.cruzi=rma.mv(yi,vi,data=ies.da1, test="t",mods=~insec-1, subset=Tryp_species=="Trypanosoma_cruzi", random= list(~ host|study_ID))
pes.da.braziliensis=rma.mv(yi,vi,data=ies.da1, mods=~insec, subset=Tryp_species=="Leishmania_braziliensis", random= list(~ host|study_ID), )
pes.da.donovani=rma.mv(yi,vi,data=ies.da1, mods=~insec, subset=Tryp_species=="Leishmania_donovani", random= list(~ host|study_ID), )
pes.da.infantum=rma.mv(yi,vi,data=ies.da1, mods=~insec, subset=Tryp_species=="Leishmania_infantum", random= list(~ host|study_ID), )
pes.da.major=rma.mv(yi,vi,data=ies.da1, mods=~insec, subset=Tryp_species=="Leishmania_major", random= list(~ host|study_ID), )
pes.da.major=rma.mv(yi,vi,data=ies.da1, mods=~insec, subset=Tryp_species=="Leishmania_major", random= list(~ host|study_ID), )
pes.da.Leishmania=rma.mv(yi,vi,data=ies.da1, mods=~insec, subset=Tryp_species=="Leishmania_spp.", random= list(~ host|study_ID), )
pes.da.tropica=rma.mv(yi,vi,data=ies.da1, mods=~insec, subset=Tryp_species==("Leishmania_tropica"), random= list(~ host|study_ID), )
pes.da.avium=rma.mv(yi,vi,data=ies.da1, mods=~insec, subset=Tryp_species==("Trypanosoma_avium"), random= list(~ host|study_ID), )
pes.da.brucei=rma.mv(yi,vi,data=ies.da1, mods=~insec, subset=Tryp_species==("Trypanosoma_brucei"), random= list(~ host|study_ID), )
pes.da.congolonse=rma.mv(yi,vi,data=ies.da1, mods=~insec, subset=Tryp_species==("Trypanosoma_congolense"), random= list(~ host|study_ID), )
pes.da.simiae=rma.mv(yi,vi,data=ies.da1, mods=~insec, subset=Tryp_species==("Trypanosoma_simiae"), random= list(~ host|study_ID), )
pes.da.trypansoma=rma.mv(yi,vi,data=ies.da1, mods=~insec, subset=Tryp_species==("Trypanosoma_spp."), random= list(~ host|study_ID), )
pes.da.vivax=rma.mv(yi,vi,data=ies.da1, mods=~insec, subset=Tryp_species==("Trypanosoma_vivax"), random= list(~ host|study_ID), )
pes.da.trypanozoon=rma.mv(yi,vi,data=ies.da1, mods=~insec, subset=Tryp_species==("Trypanozoon"),  random= list(~ host|study_ID) )
###########################
#Analyzing the data from here with the main subgroups 
pes.da.L=rma.mv(yi,vi,data=ies.da3, mods=~insec, random= list( ~Tryp_species|study_ID))
pes.da.T=rma.mv(yi,vi,data=ies.da2, mods=~insec,  random= list( ~Tryp_species|study_ID))
pes.da.TC=rma.mv(yi,vi,data=ies.da3.1, mods=~insec, random= list( ~Tryp_species|study_ID))
pes.da.Tdm=rma.mv(yi,vi,data=ies.da2, mods=~insec-1, subset=dm=="micro", random= list( ~Tryp_species|study_ID, ~host|study_ID))
pes.pes.TC=transf.ipft.hm(pes.da.Tdm$beta[1:2], targ=list(ni=ies.da2$total)) 

#pes.da.T=rma(yi,vi,data=ies.da2, mods=~insec)
pes.da.D=rma.mv(yi,vi,data=ies.da4, mods=~insec-1, random= list( ~Tryp_species|study_ID))
pes.da.D1=rma.mv(yi,vi,data=ies.da1, mods=~insec-1, subset= md=="dixenous_spp.", random= list( ~Tryp_species|study_ID))

orchard_plot(pes.da.D, mod = "insec", xlab = "infection propotion ", alpha = 0.5, 
             transfm = "tanh", angle = 0)

dev.new(width=200, height=400)
pes.pes.lall=transf.ipft.hm(pes.da.L$beta[1:2], targ=list(ni=ies.da3$total)) 
#pes.pes.Tall=transf.ipft.hm(pes.da.T$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.Tall=transf.ipft.hm(pes.da.T$beta[1:2], targ=list(ni=ies.da2$total)) 
pes.pes.D=transf.ipft.hm(pes.da.D$beta[1:2], targ=list(ni=ies.da4$total)) 
pes.pes.D1=transf.ipft.hm(pes.da.D1$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.TC=transf.ipft.hm(pes.da.TC$beta[1:2], targ=list(ni=ies.da3.1$total)) 

####################################33
#common parasite groups among insects and non-insects for dix subset
##################################

pes.pes.braziliensis=transf.ipft.hm(pes.da.braziliensis$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.donovani=transf.ipft.hm(pes.da.donovani$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.infantum=transf.ipft.hm(pes.da.infantum$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.major=transf.ipft.hm(pes.da.major$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.Leishmania=transf.ipft.hm(pes.da.Leishmania$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.tropica=transf.ipft.hm(pes.da.tropica$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.avium=transf.ipft.hm(pes.da.avium$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.brucei=transf.ipft.hm(pes.da.brucei$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.congolonse=transf.ipft.hm(pes.da.congolonse$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.simiae=transf.ipft.hm(pes.da.simiae$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.trypansoma=transf.ipft.hm(pes.da.trypansoma$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.vivax=transf.ipft.hm(pes.da.vivax$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.trypanzoon=transf.ipft.hm(pes.da.trypanozoon$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.lall=transf.ipft.hm(pes.da.L$beta[1:2], targ=list(ni=ies.da3$total)) 
pes.pes.Tall=transf.ipft.hm(pes.da.T$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.Tall=transf.ipft.hm(pes.da.T$beta[1:2], targ=list(ni=ies.da2$total)) 
pes.pes.D=transf.ipft.hm(pes.da.D$beta[1:2], targ=list(ni=ies.da4$total)) 
pes.da.D
#runing the models without model intercept.
pes.da.cruzi1=rma.mv(yi,vi,data=ies.da1, test="t", mods=~insec, subset=Tryp_species1=="Trypanosoma_cruzi", random= list(~ host|study_ID), )
pes.da.braziliensis1=rma.mv(yi,vi,data=ies.da1, test="t", mods=~insec, subset=Tryp_species1=="Leishmania_braziliensis", random= list(~ host|study_ID), )
pes.da.donovani1=rma.mv(yi,vi,data=ies.da1,test="t", mods=~insec, subset=Tryp_species1=="Leishmania_donovani", random= list(~ host|study_ID), )
pes.da.infantum1=rma.mv(yi,vi,data=ies.da1, test="t",mods=~insec, subset=Tryp_species1=="Leishmania_infantum", random= list(~ host|study_ID), )
pes.da.major1=rma.mv(yi,vi,data=ies.da1, test="t",mods=~insec, subset=Tryp_species1=="Leishmania_major", random= list(~ host|study_ID), )
pes.da.Leishmania1=rma.mv(yi,vi,data=ies.da1,test="t", mods=~insec, subset=Tryp_species1=="Leishmania_spp.", random= list(~ host|study_ID), )
pes.da.tropica1=rma.mv(yi,vi,data=ies.da1, test="t",mods=~insec, subset=Tryp_species1==("Leishmania_tropica"), random= list(~ host|study_ID), )
pes.da.avium1=rma.mv(yi,vi,data=ies.da1, test="t",mods=~insec, subset=Tryp_species1==("Trypanosoma_avium"), random= list(~ host|study_ID), )
pes.da.brucei1=rma.mv(yi,vi,data=ies.da1, test="t",mods=~insec, subset=Tryp_species1==("Trypanosoma_brucei"), random= list(~ host|study_ID), )
pes.da.congolonse1=rma.mv(yi,vi,data=ies.da1,test="t", mods=~insec, subset=Tryp_species1==("Trypanosoma_congolense"), random= list(~ host|study_ID), )
pes.da.simiae1=rma.mv(yi,vi,data=ies.da1, test="t",mods=~insec, subset=Tryp_species1==("Trypanosoma_simiae"), random= list(~ host|study_ID), )
pes.da.trypansoma1=rma.mv(yi,vi,data=ies.da1, test="t",mods=~insec, subset=Tryp_species1==("Trypanosoma_spp."), random= list(~ host|study_ID), )
pes.da.vivax1=rma.mv(yi,vi,data=ies.da1, test="t",mods=~insec, subset=Tryp_species1==("Trypanosoma_vivax"), random= list(~ host|study_ID), )
pes.da.trypanozoon1=rma.mv(yi,vi,data=ies.da1,test="t", mods=~insec, subset=Tryp_species1==("Trypanozoon") )
pes.da.L1=rma.mv(yi,vi,data=ies.da1, mods=~insec, test="t",subset=all_type2==("Leishmania_spp.") )
pes.da.T1=rma.mv(yi,vi,data=ies.da2, mods=~insec, test="t")
pes.da.D1=rma.mv(yi,vi,data=ies.da1, test="t",mods=~insec, subset=md==("dixenous_spp.") )
#backtrasforming the estimates
pes.pes.cruzi=transf.ipft.hm(pes.da.cruzi$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.braziliensis=transf.ipft.hm(pes.da.braziliensis$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.donovani=transf.ipft.hm(pes.da.donovani$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.infantum=transf.ipft.hm(pes.da.infantum$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.major=transf.ipft.hm(pes.da.major$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.Leishmania=transf.ipft.hm(pes.da.Leishmania$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.tropica=transf.ipft.hm(pes.da.tropica$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.avium=transf.ipft.hm(pes.da.avium$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.brucei=transf.ipft.hm(pes.da.brucei$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.congolonse=transf.ipft.hm(pes.da.congolonse$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.simiae=transf.ipft.hm(pes.da.simiae$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.trypansoma=transf.ipft.hm(pes.da.trypansoma$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.vivax=transf.ipft.hm(pes.da.vivax$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.trypanzoon=transf.ipft.hm(pes.da.trypanozoon$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.lall=transf.ipft.hm(pes.da.L$beta[1:2], targ=list(ni=ies.da1$total)) 
pes.pes.Tall=transf.ipft.hm(pes.da.T$beta[1:2], targ=list(ni=ies.da1$total)) 

#################################################
#generating a comparsion between each common host group among insects based on parasite life-cycle. 
data1c<-read.csv("Book3.1H1.csv", na.strings=c("", NA))
d1<-data1c[which(data1c$host=="flies"),]
d1<-d1[which(d1$all_type<="4"),]
d1<-d1[which(d1$insec=="1"),]
f.data=escalc(xi=cases, ni=total, data=d1, measure="PFT", add=0) 

t1<-data1c[which(data1c$host=="truebugs"),]
t1<-t1[which(t1$all_type<="4"),]
t1<-t1[which(t1$insec=="1"),]
t.data=escalc(xi=cases, ni=total, data=t1, measure="PFT", add=0) 

i1<-data1c[which(data1c$insec=="1"),]
i1<-i1[which(i1$all_type<="4"),]
i.data=escalc(xi=cases, ni=total, data=i1, measure="PFT", add=0) 

i3<- data1c[which(data1c$all_type<="4"),]
i3.data=escalc(xi=cases, ni=total, data=i3, measure="PFT", add=0) 

i2<-data1c[which(data1c$insec=="2"),]
i2<-i2[which(i2$all_type<="4"),]
i2.data=escalc(xi=cases, ni=total, data=i2, measure="PFT", add=0) 

i.data$host<-as.factor(i.data$host)
i.data$Tryp_species1<-as.factor(i.data$Tryp_species1)
i.data$md<-as.factor(i.data$md)

i2.data$host<-as.factor(i2.data$host)
i2.data$Tryp_species1<-as.factor(i2.data$Tryp_species1)
i2.data$md<-as.factor(i2.data$md)

t.data$host<-as.factor(t.data$host)
t.data$Tryp_species1<-as.factor(t.data$Tryp_species1)
t.data$md<-as.factor(t.data$md)
i.data$host1<-as.factor(i.data$host1)

flies.diff1=rma.mv(yi,vi, data=f.data, mods=~md-1, random= list(~Tryp_species|study_ID ))
truebug.diff1=rma.mv(yi,vi, data=t.data, mods=~md-1, random= list(~Tryp_species12|study_ID ))
insec.diff1=rma.mv(yi,vi, data=i.data, mods=~md-1, random= list(~Tryp_species1|study_ID, ~host|study_ID ))
insec.diff2=rma.mv(yi,vi, data=i.data, mods=~host-1, random= list(~Tryp_species1|study_ID, ~host|study_ID ))
insec.dm=rma.mv(yi,vi, data=i.data, mods=~dm-1,test = "t", random= list(~Tryp_species1|study_ID, ~host|study_ID))
insec.dm2=rma.mv(yi,vi, data=i2.data, mods=~dm-1,test = "t", random= list(~Tryp_species1|study_ID, ~host|study_ID))
insec.dm1.d=rma.mv(yi,vi, data=i.data, mods=~dm-1,test = "t", subset=md=="dixenous_spp.", random= list(~Tryp_species1|study_ID, ~host|study_ID))
insec.dm2.d=rma.mv(yi,vi, data=i2.data, mods=~dm-1,test = "t", subset=md=="dixenous_spp.", random= list(~Tryp_species1|study_ID, ~host|study_ID))
insec.dm1.m=rma.mv(yi,vi, data=i.data, mods=~dm-1, test = "t", subset=md=="monoxenous_spp.")

insec.dm1=rma.mv(yi,vi, data=i.data, mods=~dm,test = "t", random= list(~Tryp_species1|study_ID, ~host|study_ID))
insec.dm21=rma.mv(yi,vi, data=i2.data, mods=~dm,test = "t", random= list(~Tryp_species1|study_ID, ~host|study_ID))
insec.dm1.d1=rma.mv(yi,vi, data=i.data, mods=~dm-1,test = "t", random= list(~Tryp_species1|study_ID, ~host|study_ID))
insec.dm2.n1=rma.mv(yi,vi, data=i2.data, mods=~dm,test = "t", random= list(~Tryp_species1|study_ID, ~host|study_ID))
insec.dm1.m1=rma.mv(yi,vi, data=i.data, mods=~dm, test = "t", subset=md=="monoxenous_spp.")
pes.pes.dm1.d1=transf.ipft.hm(insec.dm2.n1$beta[1:2], targ=list(ni=i2.data$total)) 


i3.data$insec<-as.factor(i3.data$insec)

dev.new(width=200, height=400)
insec.dm_ID=rma.mv(yi,vi,data=i.data,test="t", mods=~insec, subset=Tryp_species1==("Trypanosoma_congolense"), random= list(~ host|study_ID), )
insec.insec=rma.mv(yi,vi,data=i3.data,test="t", mods=~ insec-1, random= list(~ host|study_ID, ~Tryp_species|study_ID), )

dev.new(width=200, height=400)

orchard_plot(insec.insec, mod = "insec", xlab = "infection propotion among insects ", alpha = 0.5, 
             transfm = "tanh", angle = 0)
flies.diff=rma.mv(yi,vi, data=f.data, mods=~md, random= list(~Tryp_species|study_ID ))
truebug.diff=rma.mv(yi,vi, data=t.data, mods=~md-1, random= list(~Tryp_species|study_ID ))
insec.diff=rma.mv(yi,vi, data=i.data, mods=~md, random= list(~Tryp_species|study_ID ))

pes.pes.flies=transf.ipft.hm(flies.diff$beta[1:2], targ=list(ni=f.data$total)) 
pes.pes.truebug=transf.ipft.hm(truebug.diff$beta[1:2], targ=list(ni=t.data$total)) 
pes.pes.insec=transf.ipft.hm(insec.diff$beta[1:6], targ=list(ni=i.data$total)) 
pes.pes.insec1=transf.ipft.hm(insec.diff2$beta[1:6], targ=list(ni=i.data$total)) 

pes.pes.dm1=transf.ipft.hm(insec.dm$beta[1:3], targ=list(ni=i.data$total)) 
pes.pes.dm2=transf.ipft.hm(insec.dm2$beta[1:3], targ=list(ni=i.data$total)) 
pes.pes.dm1.d=transf.ipft.hm(insec.dm1.d$beta[1:3], targ=list(ni=i.data$total)) 
pes.pes.dm2.d=transf.ipft.hm(insec.dm2.d$beta[1:3], targ=list(ni=i.data$total)) 
pes.pes.dm1.m=transf.ipft.hm(insec.dm1.m$beta[1:2], targ=list(ni=i.data$total)) 
###########################################################3
data1c<-read.csv("Book3.1H1.csv", na.strings=c("", NA))
ies.da1=escalc(xi=cases, ni=total, data=data1c, measure="PFT", add=0) 
#MAking a subset for only bees 
b1<-ies.da1[which(ies.da1$host=="bees"),]
b1.data=escalc(xi=cases, ni=total, data=b1, measure="PFT", add=0) 
b1.data$host<-as.factor(b1.data$host)
b1.data$Tryp_species1<-as.factor(b1.data$Tryp_species1)
b1.data$md<-as.factor(b1.data$md)
b1.data$all_type2<-as.factor(b1.data$all_type2)
b1.data$year<-as.factor(b1.data$year)
b1.data$country<-as.factor(b1.data$country)
b1.data$dm<-as.factor(b1.data$dm)
b1.data$host_type<-as.factor(b1.data$host_type)
b1.data$host_species1<-as.factor(b1.data$host_species1)
b1.data$genus1<-as.factor(b1.data$genus1)
b1.data$WM<-as.factor(b1.data$WM)
#assessing how diff prev is among wild vs managed bees  of alh honeybees 
WM_bees1=rma.mv(yi,vi,data=b1.data, mods= ~WM-1, subset=genus1=="Apis_spp.", random= list(~Tryp_species1|study_ID, ~host_species1|study_ID))
pes.bees1=transf.ipft.hm(WM_bees1$beta[1:2], targ=list(ni=b1.data$total)) 
#assessing how diff prev is among wild vs managed bees  of all Bombus_spp. 
dev.new(width=200, height=400)

orchard_plot(WM_all_bees, xlab = "infection propotion among all bees ", alpha = 0.5,transfm = "tanh", angle = 0)

WM_bees2=rma.mv(yi,vi,data=b1.data, mods= ~WM-1, subset=genus1=="Bombus_spp.", random= list(~Tryp_species1|study_ID, ~host_species1|study_ID ))
pes.bees2=transf.ipft.hm(WM_bees2$beta[1:2], targ=list(ni=b1.data$total)) 
#assessing how diff prev is among wild vs managed bees  of all bees 
orchard_plot(WM_bees1, xlab = "infection propotion among all bees ", alpha = 0.5,transfm = "tanh", angle = 0)

WM_all_bees=rma.mv(yi,vi,data=b1.data, mods= ~WM-1, random= list(~Tryp_species1|study_ID))
pes.all_bees=transf.ipft.hm(WM_all_bees$beta[1:2], targ=list(ni=b1.data$total)) 
#####################################3
#trying to understand source of heterogeneity 
#####################################
expoeki=rma.mv(yi,vi,data=b1.data, mods= ~host_species1-1, subset=Tryp_species1=="Crithidia_expoeki")
bombi=rma.mv(yi,vi,data=b1.data, mods= ~host_species1-1, subset=Tryp_species1=="Crithidia_bombi")
mexicana=rma.mv(yi,vi,data=b1.data, mods= ~host_species1-1, subset=Tryp_species1=="Crithidia_mexicana")
crithidia=rma.mv(yi,vi,data=b1.data, mods= ~host_species1-1, subset=Tryp_species1=="Crithidia_spp.")


pes.expoeki=transf.ipft.hm(expoeki$beta[1:10], targ=list(ni=b1.data$total)) 
pes.bombi=transf.ipft.hm(bombi$beta[1:40], targ=list(ni=b1.data$total)) 
pes.mexicana=transf.ipft.hm(mexicana$beta[1:8], targ=list(ni=b1.data$total)) 
pes.crithidia=transf.ipft.hm(crithidia$beta[1:7], targ=list(ni=b1.data$total)) 

#data for L.passim only
bs1<-ies.da1[which(ies.da1$Tryp_species1=="Lotmaria_passim"),]
bs.1.data=escalc(xi=cases, ni=total, data=bs1, measure="PFT", add=0) 
bs.1.data$host<-as.factor(bs.1.data$host)
bs.1.data$Tryp_species1<-as.factor(bs.1.data$Tryp_species1)
bs.1.data$md<-as.factor(bs.1.data$md)
bs.1.data$all_type2<-as.factor(bs.1.data$all_type2)
bs.1.data$year<-as.factor(bs.1.data$year)
bs.1.data$country<-as.factor(bs.1.data$country)
bs.1.data$dm<-as.factor(bs.1.data$dm)

bees1=rma.uni(yi,vi,data=c1.data, subset=Tryp_species1=="Lotmaria_passim")
bees1.1=rma.uni(yi,vi,data=c1.data, mods=~year-1, subset=Tryp_species1=="Lotmaria_passim")
bees1.2=rma.uni(yi,vi,data=c1.data, mods=~country-1, subset=Tryp_species1=="Lotmaria_passim")
bees1.3=rma.uni(yi,vi,data=c1.data, mods=~year+country-1, subset=Tryp_species1=="Lotmaria_passim")
bees1.4=rma.uni(yi,vi,data=c1.data, mods=~dm-1, subset=Tryp_species1=="Lotmaria_passim")

bees1.2=rma.uni(yi,vi,data=bs.1.data, mods=~country)


bees2=rma.uni(yi,vi,data=b1.data, mods=subset=Tryp_species1=="Crithidia_bombi")
bees2.1=rma.uni(yi,vi,data=c1.data, mods=~year-1, subset=Tryp_species1=="Crithidia_bombi")
bees2.2=rma.uni(yi,vi,data=c1.data, mods=~year+country-1, subset=Tryp_species1=="Crithidia_bombi")
bees2.3=rma.uni(yi,vi,data=c1.data, mods=~country-1, subset=Tryp_species1=="Crithidia_bombi")
bees2.4=rma.uni(yi,vi,data=c1.data, mods=~dm-1, subset=Tryp_species1=="Crithidia_bombi")

bees3=rma.uni(yi,vi,data=c1.data, subset=Tryp_species1=="Crithidia_spp.")
bees3.1=rma.uni(yi,vi,data=c1.data, mods=~year-1, subset=Tryp_species1=="Crithidia_spp.")
bees3.2=rma.uni(yi,vi,data=c1.data, mods=~year+country-1, subset=Tryp_species1=="Crithidia_spp.")
bees3.3=rma.uni(yi,vi,data=c1.data, mods=~country-1, subset=Tryp_species1=="Crithidia_spp.")
bees3.4=rma.uni(yi,vi,data=c1.data, mods=~dm-1, subset=Tryp_species1=="Crithidia_spp.")

bees3=rma.uni(yi,vi,data=c1.data, subset=Tryp_species1=="Crithidia_spp.")
bees3.1=rma.uni(yi,vi,data=c1.data, mods=~year-1, subset=Tryp_species1=="Crithidia_spp.")
bees3.2=rma.uni(yi,vi,data=c1.data, mods=~year+country-1, subset=Tryp_species1=="Crithidia_spp.")
bees3.3=rma.uni(yi,vi,data=c1.data, mods=~country-1, subset=Tryp_species1=="Crithidia_spp.")

bees4=rma.uni(yi,vi,data=c1.data, subset=Tryp_species1=="Crithidia_expoeki")
bees4.1=rma.uni(yi,vi,data=c1.data, mods=~year-1, subset=Tryp_species1=="Crithidia_expoeki")
bees4.2=rma.uni(yi,vi,data=c1.data, mods=~year+country-1, subset=Tryp_species1=="Crithidia_expoeki")
bees4.3=rma.uni(yi,vi,data=c1.data, mods=~country-1, subset=Tryp_species1=="Crithidia_expoeki")


bees5=rma.uni(yi,vi,data=c1.data, subset=Tryp_species1=="Crithidia_mexicana")
bees5.1=rma.uni(yi,vi,data=c1.data, mods=~year-1, subset=Tryp_species1=="Crithidia_mexicana")
bees5.2=rma.uni(yi,vi,data=c1.data, mods=~year+country-1, subset=Tryp_species1=="Crithidia_mexicana")
bees5.3=rma.uni(yi,vi,data=c1.data, mods=~country-1, subset=Tryp_species1=="Crithidia_mexicana")

bees6=rma.uni(yi,vi,data=c1.data, subset=Tryp_species1=="Crithidia_mellificae,_Lotmaria_passim")
bees6.1=rma.uni(yi,vi,data=c1.data, mods=~year-1, subset=Tryp_species1=="Crithidia_mellificae,_Lotmaria_passim")
bees6.2=rma.uni(yi,vi,data=c1.data, mods=~year+country-1, subset=Tryp_species1=="Crithidia_mellificae,_Lotmaria_passim")
bees6.3=rma.uni(yi,vi,data=c1.data, mods=~country-1, subset=Tryp_species1=="Crithidia_mellificae,_Lotmaria_passim")



mono<-read.csv("Book15.csv", na.strings=c("", NA))
c1<-mono[which(mono$Tryp_species1=="Crithidia_bombi"),]
c1.1=escalc(xi=cases, ni=total, data=d1, measure="PFT", add=0) 

c1.1$host<-as.factor(c1.1$host)
c1.1$Tryp_species1<-as.factor(c1.1$Tryp_species1)
c1.1$md<-as.factor(c1.1$md)
c1.1$all_type2<-as.factor(c1.1$all_type2)

c1.1$year<-as.factor(c1.1$year)
c1.1$country<-as.factor(c1.1$country)

crithidia_bombiA=rma.uni(yi,vi,data=c1.1, subset=country=="United States", random=list(~1|study_ID))
crithidia_bombiB=rma.uni(yi,vi,data=c1.1, subset=country=="Switzerland")
crithidia_bombiC=rma.uni(yi,vi,data=c1.1, subset=country=="United Kingdom")
crithidia_bombiD=rma.uni(yi,vi,data=c1.1, subset=country=="India")

crithidia_bombi.1=rma.uni(yi,vi,data=c1.1, mods=~year-1 ,subset=country=="United Kingdom")
crithidia_bombi.2=rma.uni(yi,vi,data=c1.1, mods=~year-1 ,subset=country=="Switzerland")
crithidia_bombi.3=rma.uni(yi,vi,data=c1.1, mods=~year-1 ,subset=country=="Switzerland")


es1=rma.uni(yi,vi,data=c1.data, subset=Tryp_species1=="Lotmaria_passim")

bees1.1=rma.uni(yi,vi,data=c1.data, mods=~year-1, subset=Tryp_species1=="Lotmaria_passim")
bees1.2=rma.uni(yi,vi,data=c1.data, mods=~country-1, subset=Tryp_species1=="Lotmaria_passim")
bees1.3=rma.uni(yi,vi,data=c1.data, mods=~year+country-1, subset=Tryp_species1=="Lotmaria_passim")
bees1.4=rma.uni(yi,vi,data=c1.data, mods=~dm-1, subset=Tryp_species1=="Lotmaria_passim")

bees2=rma.uni(yi,vi,data=c1.data, subset=Tryp_species1=="Crithidia_bombi")
bees2.1=rma.uni(yi,vi,data=c1.data, mods=~year-1, subset=Tryp_species1=="Crithidia_bombi")
bees2.2=rma.uni(yi,vi,data=c1.data, mods=~year+country-1, subset=Tryp_species1=="Crithidia_bombi")
bees2.3=rma.uni(yi,vi,data=c1.data, mods=~country-1, subset=Tryp_species1=="Crithidia_bombi")
bees2.4=rma.uni(yi,vi,data=c1.data, mods=~dm-1, subset=Tryp_species1=="Crithidia_bombi")

bees3=rma.uni(yi,vi,data=c1.data, subset=Tryp_species1=="Crithidia_spp.")
bees3.1=rma.uni(yi,vi,data=c1.data, mods=~year-1, subset=Tryp_species1=="Crithidia_spp.")
bees3.2=rma.uni(yi,vi,data=c1.data, mods=~year+country-1, subset=Tryp_species1=="Crithidia_spp.")
bees3.3=rma.uni(yi,vi,data=c1.data, mods=~country-1, subset=Tryp_species1=="Crithidia_spp.")
bees3.4=rma.uni(yi,vi,data=c1.data, mods=~dm-1, subset=Tryp_species1=="Crithidia_spp.")
summary (bees1.1)
summary (bees1.2)

bees3=rma.uni(yi,vi,data=c1.data, subset=Tryp_species1=="Crithidia_spp.")
bees3.1=rma.uni(yi,vi,data=c1.data, mods=~year-1, subset=Tryp_species1=="Crithidia_spp.")
bees3.2=rma.uni(yi,vi,data=c1.data, mods=~year+country-1, subset=Tryp_species1=="Crithidia_spp.")
bees3.3=rma.uni(yi,vi,data=c1.data, mods=~country-1, subset=Tryp_species1=="Crithidia_spp.")

bees4=rma.uni(yi,vi,data=c1.data, subset=Tryp_species1=="Crithidia_expoeki")
bees4.1=rma.uni(yi,vi,data=c1.data, mods=~year-1, subset=Tryp_species1=="Crithidia_expoeki")
bees4.2=rma.uni(yi,vi,data=c1.data, mods=~year+country-1, subset=Tryp_species1=="Crithidia_expoeki")
bees4.3=rma.uni(yi,vi,data=c1.data, mods=~country-1, subset=Tryp_species1=="Crithidia_expoeki")


bees5=rma.uni(yi,vi,data=c1.data, subset=Tryp_species1=="Crithidia_mexicana")
bees5.1=rma.uni(yi,vi,data=c1.data, mods=~year-1, subset=Tryp_species1=="Crithidia_mexicana")
bees5.2=rma.uni(yi,vi,data=c1.data, mods=~year+country-1, subset=Tryp_species1=="Crithidia_mexicana")
bees5.3=rma.uni(yi,vi,data=c1.data, mods=~country-1, subset=Tryp_species1=="Crithidia_mexicana")

bees6=rma.uni(yi,vi,data=c1.data, subset=Tryp_species1=="Crithidia_mellificae,_Lotmaria_passim")
bees6.1=rma.uni(yi,vi,data=c1.data, mods=~year-1, subset=Tryp_species1=="Crithidia_mellificae,_Lotmaria_passim")
bees6.2=rma.uni(yi,vi,data=c1.data, mods=~year+country-1, subset=Tryp_species1=="Crithidia_mellificae,_Lotmaria_passim")
bees6.3=rma.uni(yi,vi,data=c1.data, mods=~country-1, subset=Tryp_species1=="Crithidia_mellificae,_Lotmaria_passim")


