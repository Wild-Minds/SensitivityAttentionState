library(readxl)
library(languageR)
library(lattice)
library(lme4)
library(dplyr)
library(ggplot2)
library(forcats)
library(scales)

library(lme4)
library(MuMIn)
library(MASS)
library(stats)
library(dplyr)


# .1) Sequence -------------------------------------------------------------

#See file "Data0_Inf_Juv_Ado_OnlyData"

# .2) Unimodal adjustment -------------------------------------------------------------

#__GLMER mismatching -------------------------------------------------------------

Umismatch <- read_excel(".../Umismatch.xlsx") #Import file "Umismatch"

Umismatch$Signaller <- as.factor(Umismatch$Signaller)
Umismatch$Age_Range <- as.factor(Umismatch$`Age Range`)
Umismatch$Age_Range <-fct_relevel(Umismatch$Age_Range, c("infant", "juvenile","adolescent"))
Umismatch$Age_Range <- factor(Umismatch$Age_Range, labels=c("Infant", "Juvenile","Adolescent"))

Umismatch$Sex <- as.factor(Umismatch$Sex)
Umismatch$Age <- as.numeric(Umismatch$Age)
Umismatch$UnimodMatch <- as.numeric(Umismatch$UnimodMatch)
Umismatch$UnimodMatch2 <- as.numeric(Umismatch$UnimodMatch2)
Umismatch$UnimodMismatch <- as.numeric(Umismatch$UnimodMismatch)
Umismatch$UnimodMismatch2 <- as.numeric(Umismatch$UnimodMismatch2)
Umismatch$NumVisuTot <- as.numeric(Umismatch$NumVisuTot)
Umismatch$NumVisuTot2 <- as.numeric(Umismatch$NumVisuTot2) 


glmer1 <- glmer(UnimodMismatch~1+(1|Signaller), family=poisson, data=Umismatch)
glmer2 <- glmer(UnimodMismatch~Age+(1|Signaller), family=poisson, data=Umismatch)
glmer3 <- glmer(UnimodMismatch~Sex+(1|Signaller), family=poisson, data=Umismatch)


library(AICcmodavg)
models<-list(glmer1,glmer2,glmer3)
mod.names <- c('1.Null', '2.Age', '3.Sex')
aictab(cand.set = models, modnames = mod.names)
anova(glmer1,glmer2)


summary(glmer2)
a<-confint(glmer2, method="Wald")
a

#__Correlation mismatching -------------------------------------------------------------


library(Kendall)

Data<- read_excel(".../Data1_OSF.xlsx") #Import file "Data1_OSF"

Data$Signaller <- as.factor(Data$Signaller)
Data$Age_Range <- as.factor(Data$`Age Range`)
Data$Age_Range <-fct_relevel(Data$Age_Range, c("infant", "juvenile","ado"))
Data$Age_Range <- factor(Data$Age_Range, labels=c("Infant", "Juvenile","Adolescent"))

Data$Sex <- as.factor(Data$Sex)
Data$BetwAge <- as.numeric(Data$BetwAge)
Data$TotalG <- as.numeric(Data$`TotalG All Con`)
Data$Visual_Att <- factor(Data$Visual_Att, labels=c("Mother shows visual attention","Mother shows visual inattention"))
Data$Modes <- as.factor(Data$Modes)
Data$Modes <-fct_relevel(Data$Modes, c("Silent gestures", "Audible_OR_Contact","Audible gestures","Contact gestures"))
Data$Modes <- factor(Data$Modes, labels=c("Silent-visual gestures", "Audible-or-contact gestures","Audible gestures","Contact gestures"))

Data$Count <- as.numeric(Data$Count)
Data$TotalG_Con<- as.numeric(Data$`TotalG Con`)
Data$TotalVisu_ID<- as.numeric(Data$TotalVisu_ID)
Data$TotalAG_ID<- as.numeric(Data$TotalAG_ID)
Data <-mutate(Data, FreqS=Count/TotalG_Con)

DatasetID_Visu<-filter(Data,Modes=="Silent-visual gestures")
DatasetID_Visu <-mutate(DatasetID_Visu, FreqSVisu=Count/TotalVisu_ID)

res1<-cor.test(DatasetID_Visu$BetwAge,DatasetID_Visu$FreqSVisu, method="spearman")
res1

DataVisuAtt<-filter(DatasetID_Visu, Visual_Att=="Mother shows visual attention")
res2<-cor.test(DataVisuAtt$BetwAge,DataVisuAtt$FreqSVisu, method="spearman")
res2

DataVisuInatt<-filter(DatasetID_Visu,Visual_Att=="Mother shows visual inattention")
res3<-cor.test(DataVisuInatt$BetwAge,DataVisuInatt$FreqSVisu, method="spearman")
res3

# Fig1 --------------------------------------------------------------------
Fig1<-ggplot(DatasetID_Visu, aes(x=BetwAge, y=FreqSVisu, shape=Visual_Att)) +
  geom_point(aes(shape=Visual_Att), size=4) +
  scale_shape_manual(values=c(17, 19))+
  scale_y_continuous(labels = percent)+
  #scale_color_manual(values=c('#999999','#E69F00'))+
  geom_smooth(aes(linetype=Visual_Att),color="black",method=lm, se=TRUE, fullrange=TRUE)+
  scale_x_continuous(breaks=seq(0, 180, by = 20))+
  xlab("Age mean (in months)") +
  ylab("Percentage of silent-visual gestures") +
  theme(text = element_text(family = "Optima",face = "bold", size = 14),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        axis.line = element_line(colour = "black", 
                                 size = 1, linetype = "solid"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x = element_text(face="bold", 
                                   size=14),
        axis.text.y = element_text(face="bold", 
                                   size=14),
        legend.title = element_blank(),
        legend.text = element_text(size=12,face = "plain"))
Fig1

#__Age effect with Fisher -------------------------------------------------------------

a<-DatasetID_Visu %>% filter(Visual_Att=="Visual inattention",Age_Range=="Infant")
a<-sum(a$Count)
b<-DatasetID_Visu %>% filter(Visual_Att=="Visual attention",Age_Range=="Infant")
b<-sum(b$Count)


c<-DatasetID_Visu %>% filter(Visual_Att=="Visual inattention",Age_Range=="Juvenile")
c<-sum(c$Count)
d<-DatasetID_Visu %>% filter(Visual_Att=="Visual attention",Age_Range=="Juvenile")
d<-sum(d$Count)

e<-DatasetID_Visu %>% filter(Visual_Att=="Visual inattention",Age_Range=="Adolescent")
e<-sum(e$Count)
f<-DatasetID_Visu %>% filter(Visual_Att=="Visual attention",Age_Range=="Adolescent")
f<-sum(f$Count)


Infant<-c(a,b)
Juvenile<-c(c,d)
Ado<-c(e,f)

data1<-rbind(Infant,Juvenile)
data2<-rbind(Juvenile,Ado)
data3<-rbind(Infant,Ado)
data123<-rbind(data1,data2,data3)
ofA3<-fisher.test(data123,workspace = 2000000)

afA3<-fisher.test(data1,workspace = 2000000)
bfA3<-fisher.test(data2,workspace = 2000000)
cfA3<-fisher.test(data3,workspace = 2000000)

pvalues <- c(ofA3$p.value,afA3$p.value,bfA3$p.value,cfA3$p.value)
p.adjust(pvalues,method="bonferroni")


# Fig2 --------------------------------------------------------------------

DatasetID_Visu$Visual_Att <- factor(DatasetID_Visu$Visual_Att, labels=c("Visual attention","Visual inattention"))
DatasetID_Visu$Visual_Att <-fct_relevel(DatasetID_Visu$Visual_Att, c("Visual inattention","Visual attention"))


A1_infant <-ggplot(DatasetID_Visu[DatasetID_Visu$Age_Range == "Infant", ]) +
  aes(x = Visual_Att, y = Count, group = Signaller) +
  geom_line(aes(linetype = Signaller), size = 1) +
  xlab("Maternal visual attention") +
  scale_y_continuous(name="Number of signals", limits=c(0, 20))+
  labs(fill = "Signaller")+
  theme_classic()+
  theme(axis.text=element_text(size=14), 
        axis.title.y=element_text(size=14,face="bold"),
        axis.title.x=element_blank())+
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=12,face="bold"))
A1_infant


A1_juvenile <-ggplot(DatasetID_Visu[DatasetID_Visu$Age_Range == "Juvenile", ]) +
  aes(x = Visual_Att, y = Count, group = Signaller) +
  geom_line(aes(linetype = Signaller), size = 1) +
  xlab("Maternal visual attention") +
  scale_y_continuous(name="Number of signals", limits=c(0, 20))+
  labs(fill = "Signaller")+
  theme_classic()+
  theme(axis.text=element_text(size=14), 
        axis.title.y=element_text(size=14,face="bold"),
        axis.title.x=element_blank())+
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=12,face="bold"))
A1_juvenile

A1_adolescent <-ggplot(DatasetID_Visu[DatasetID_Visu$Age_Range == "Adolescent", ]) +
  aes(x = Visual_Att, y = Count, group = Signaller) +
  geom_line(aes(linetype = Signaller), size = 1) +
  xlab("Maternal visual attention") +
  scale_y_continuous(name="Number of signals", limits=c(0, 20))+
  labs(fill = "Signaller")+
  theme_classic()+
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.text=element_text(size=12),
        legend.title=element_text(size=12,face="bold"))
A1_adolescent


library(cowplot)
Fig2<-plot_grid(A1_infant, A1_juvenile, A1_adolescent,labels = c("A", "B", "C"),nrow = 3, align = "hv" )
Fig2


# .3) Crossmodal adjustment -------------------------------------------------------------

#__Correlation-------------------------------------------------------------
DatasetCrossMod<-filter(Data,Modes=="Audible-or-contact gestures"|Modes=="Silent-visual gestures")

test2<-ggplot(DatasetCrossMod, aes(x=BetwAge, y=FreqS, color=Modes, shape=Modes)) +
  geom_point() + 
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE)+
  facet_grid(~Visual_Att)+
  theme_classic()
test2


DatasetCMAtt_AG<-filter(DatasetCrossMod,Modes=="Audible-or-contact gestures", Visual_Att=="Mother shows visual attention")
resa<-cor.test(DatasetCMAtt_AG$BetwAge,DatasetCMAtt_AG$FreqS, method="spearman")
resa

DatasetCMAtt_Visu<-filter(DatasetCrossMod,Modes=="Silent-visual gestures", Visual_Att=="Mother shows visual attention")
resb<-cor.test(DatasetCMAtt_Visu$BetwAge,DatasetCMAtt_Visu$FreqS, method="spearman")
resb

DatasetCMinAtt_AG<-filter(DatasetCrossMod,Modes=="Audible-or-contact gestures", Visual_Att=="Mother shows visual inattention")
resc<-cor.test(DatasetCMinAtt_AG$BetwAge,DatasetCMinAtt_AG$FreqS, method="spearman")
resc

DatasetCMinAtt_Visu<-filter(DatasetCrossMod,Modes=="Audible-or-contact gestures", Visual_Att=="Mother shows visual inattention")
resd<-cor.test(DatasetCMinAtt_Visu$BetwAge,DatasetCMinAtt_Visu$FreqS, method="spearman")
resd

pvalues <- c(resa$p.value,resc$p.value)
p.adjust(pvalues,method="bonferroni")


#__Fisher by age range -------------------------------------------------------------

#All
ACINATT<-DatasetCrossMod %>% filter(Visual_Att=="Mother shows visual inattention",Modes=="Audible-or-contact gestures",)
ACINATT_All<-sum(ACINATT$Count)
ACATT<-DatasetCrossMod %>% filter(Visual_Att=="Mother shows visual attention",Modes=="Audible-or-contact gestures",)
ACATT_All<-sum(ACATT$Count)

SVINATT<-DatasetCrossMod %>% filter(Visual_Att=="Mother shows visual inattention",Modes=="Silent-visual gestures",)
SVINATT_All<-sum(SVINATT$Count)
SVATT<-DatasetCrossMod %>% filter(Visual_Att=="Mother shows visual attention",Modes=="Silent-visual gestures",)
SVATT_All<-sum(SVATT$Count)

#Infant
ACINATT_Inf<-ACINATT %>% filter(Age_Range=="Infant")
ACINATT_Inf<-sum(ACINATT_Inf$Count)
ACATT_Inf<-ACATT %>% filter(Age_Range=="Infant")
ACATT_Inf<-sum(ACATT_Inf$Count)

SVINATT_Inf<-SVINATT %>% filter(Age_Range=="Infant")
SVINATT_Inf<-sum(SVINATT_Inf$Count)
SVATT_Inf<-SVATT %>% filter(Age_Range=="Infant")
SVATT_Inf<-sum(SVATT_Inf$Count)


#Juvenile
ACINATT_Juv<-ACINATT %>% filter(Age_Range=="Juvenile")
ACINATT_Juv<-sum(ACINATT_Juv$Count)
ACATT_Juv<-ACATT %>% filter(Age_Range=="Juvenile")
ACATT_Juv<-sum(ACATT_Juv$Count)

SVINATT_Juv<-SVINATT %>% filter(Age_Range=="Juvenile")
SVINATT_Juv<-sum(SVINATT_Juv$Count)
SVATT_Juv<-SVATT %>% filter(Age_Range=="Juvenile")
SVATT_Juv<-sum(SVATT_Juv$Count)


#Adolescent
ACINATT_Ado<-ACINATT %>% filter(Age_Range=="Adolescent")
ACINATT_Ado<-sum(ACINATT_Ado$Count)
ACATT_Ado<-ACATT %>% filter(Age_Range=="Adolescent")
ACATT_Ado<-sum(ACATT_Ado$Count)

SVINATT_Ado<-SVINATT %>% filter(Age_Range=="Adolescent")
SVINATT_Ado<-sum(SVINATT_Ado$Count)
SVATT_Ado<-SVATT %>% filter(Age_Range=="Adolescent")
SVATT_Ado<-sum(SVATT_Ado$Count)

#Tests
AC<-c(ACINATT_All,ACATT_All)
Silent<-c(SVINATT_All,SVATT_All)
dataTact<-rbind(AC,Silent)

ACInf<-c(ACINATT_Inf,ACATT_Inf)
SilentInf<-c(SVINATT_Inf,SVATT_Inf)
dataTactInf<-rbind(ACInf,SilentInf)

ACJuv<-c(ACINATT_Juv,ACATT_Juv)
SilentJuv<-c(SVINATT_Juv,SVATT_Juv)
dataTactJuv<-rbind(ACJuv,SilentJuv)

ACAdo<-c(ACINATT_Ado,ACATT_Ado)
SilentAdo<-c(SVINATT_Ado,SVATT_Ado)
dataTactAdo<-rbind(ACAdo,SilentAdo)

afC1<-fisher.test(dataTactInf,workspace = 2000000)
afC1
bfC1<-fisher.test(dataTactJuv,workspace = 2000000)
bfC1
cfC1<-fisher.test(dataTactAdo,workspace = 2000000)
cfC1

pvalues <- c(afC1$p.value,bfC1$p.value,cfC1$p.value)
p.adjust(pvalues,method="bonferroni")

#Fig3 -------------------------------------------------------------

data <- read_excel(".../Data2_OSF.xlsx") #Import file "Data2_OSF"
data$Signaller <- factor(data$Signaller)
data$Age_Range <- factor(data$`Age Range`, labels = c("Adolescent", "Infant", "Juvenile"))
data$Sex <- factor(data$Sex)
data$TotalG <- as.numeric(data$`TotalG`)
data$Visual_Att <- factor(data$Visual_Att)
data$Modes <- factor(data$Modes)
data$Modes <-fct_relevel(data$Modes, c("Audible_OR_Contact","Silent gestures","Audible gestures","Contact gestures"))
data$Modes <- factor(data$Modes, labels=c("Audible-or-contact gestures","Silent-visual gestures", "Audible gestures","Contact gestures"))
data$TotalG <- as.numeric(data$`TotalG Con_AR`)
data$alpha<- as.numeric(data$alpha)
data$beta<- as.numeric(data$beta)
data$VariationB<- as.numeric(data$VariationB)


dataVarB <- data[data$Modes %in% c("Audible-or-contact gestures", "Silent-visual gestures"),
                 c("Signaller", "Age_Range", "Visual_Att", "Modes", "Count", "VariationB")]
dataVarB

dataVarB2 <- dataVarB %>%
  group_by(Visual_Att, Modes, Age_Range) %>%
  summarise(moy = mean(VariationB), et = sd(VariationB),ES = sd(VariationB)/sqrt(n()))
dataVarB2$Age_Range <- fct_relevel(dataVarB2$Age_Range, c("Infant", "Juvenile","Adolescent"))
#data3<-filter(data3, Visual_Att == "Inattentive")
dataVarB2

Fig3<-ggplot(dataVarB2) +
  aes(x = Visual_Att, y = moy, fill = Modes) +
  geom_bar(position = "dodge", color="black", stat = "identity") +
  geom_errorbar(aes(ymin = moy-ES, ymax = moy+ES, group = Modes), width=.2, position = position_dodge(0.9)) +
  facet_wrap(~ Age_Range, ncol=3) +
  xlab("Maternal visual attention") +
  ylab("Pourcentage") +
  labs(fill = "Modes")+
  scale_y_continuous(name="% varition in use", limits=c(-100, 100))+
  scale_fill_manual(values = c("#000000", "#FFFFFF"))+
  theme_classic()+
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=14,face="bold"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12,face="bold"),
        strip.text = element_text(size = 15,face="bold"))
Fig3


# .4) Modal preference -------------------------------------------------------------

DatasetPref<-filter(Data,Modes=="Contact gestures"|Modes=="Audible gestures")
DatasetPref<-filter(DatasetPref,Visual_Att=="Mother shows visual inattention" )
DatasetPref <-mutate(DatasetPref, FreqSAG=Count/TotalAG_ID)

#All
AudibINATT<-DatasetPref %>% filter(Modes=="Audible gestures")
AudibINATT_All<-sum(AudibINATT$Count)
ContactINATT<-DatasetPref %>% filter(Modes=="Contact gestures")
ContactINATT_All<-sum(ContactINATT$Count)


#Infant
AudibINATT_Inf<-AudibINATT %>% filter(Age_Range=="Infant")
AudibINATT_Inf<-sum(AudibINATT_Inf$Count)
ContactINATT_Inf<-ContactINATT %>% filter(Age_Range=="Infant")
ContactINATT_Inf<-sum(ContactINATT_Inf$Count)

#Juvenile
AudibINATT_Juv<-AudibINATT %>% filter(Age_Range=="Juvenile")
AudibINATT_Juv<-sum(AudibINATT_Juv$Count)
ContactINATT_Juv<-ContactINATT %>% filter(Age_Range=="Juvenile")
ContactINATT_Juv<-sum(ContactINATT_Juv$Count)

#Adolescent
AudibINATT_Ado<-AudibINATT %>% filter(Age_Range=="Adolescent")
AudibINATT_Ado<-sum(AudibINATT_Ado$Count)
ContactINATT_Ado<-ContactINATT %>% filter(Age_Range=="Adolescent")
ContactINATT_Ado<-sum(ContactINATT_Ado$Count)

#Tests
INATTInf<-c(AudibINATT_Inf,ContactINATT_Inf)
INATTJuv<-c(AudibINATT_Juv,ContactINATT_Juv)
INATTAdo<-c(AudibINATT_Ado,ContactINATT_Ado)
INATTInfJuvAdo<-rbind(INATTInf,INATTJuv,INATTAdo)
bfC3<-fisher.test(INATTInfJuvAdo,workspace = 2000000)


INATTInfJuv<-rbind(INATTInf,INATTJuv)
INATTJuvAdo<-rbind(INATTJuv,INATTAdo)
INATTInfAdo<-rbind(INATTInf,INATTAdo)

bfC4<-fisher.test(INATTInfJuv,workspace = 2000000)
bfC5<-fisher.test(INATTJuvAdo,workspace = 2000000)
bfC6<-fisher.test(INATTInfAdo,workspace = 2000000)

pvalues <- c(bfC3$p.value,bfC4$p.value,bfC5$p.value,bfC6$p.value)
p.adjust(pvalues,method="bonferroni")


#Fig4 -------------------------------------------------------------

Fig4<-ggplot(DatasetPref, aes(Modes,FreqSAG)) +
  geom_boxplot(aes(linetype=Modes),fill='white', color="black", size = 1.25) +
  ylim(0,1)+
  geom_jitter(shape=16, position=position_jitter(0.2),size=4)+
  scale_y_continuous(labels = percent)+
  facet_grid(~Age_Range)+
  xlab("Age range") +
  ylab("Percentage of gestures") +
  theme(text = element_text(family = "Optima",face = "bold", size = 14),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        size = 0.5, linetype = "solid"),
        axis.line= element_line(colour = "black", 
                                size = 1, linetype = "solid"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x = element_text(face="bold",
                                   colour = "white",
                                   size=14),
        axis.text.y = element_text(face="bold", 
                                   size=14),
        legend.title = element_blank(),
        legend.text = element_text(size=14,face = "plain"),
        strip.background = element_rect(colour="white", fill="white", 
                                        size=3, linetype="solid"),
        strip.text.x = element_text(size=20))
Fig4
