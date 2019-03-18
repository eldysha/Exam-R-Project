#### PUNTO 0 ####prepocessing-----------------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(Hmisc)
library(corrgram)
library(corrplot)
library(psych)
library(ggpubr)
library(tseries)

rm(list=ls())

progetto_cavalli_totale<-read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/horse-colic/horse-colic.data"),
                                  header=FALSE, sep = "") #importa il dataset
cavalli<- progetto_cavalli_totale

colnames(cavalli)<-
  c("surgery","age","hospotal_number","rectal_temperature","pulse", "respiratory_rate","temperature of extremities", "peripheral pulse", "mucous membranes", "capillary_refill_time", "pain", "peristalsis",
    "abdominal distension","nasogastric tube","nasogastric_reflux","nasogastric reflux PH","rectal examination - feces","abdomen","packed_cell_volume","total protein","abdominocentesis appearance","abdomcentesis total protein",
    "outcome", "surgical_lesion", "type of lesion 1","type of lesion 2", "type of lesion 3", "cp_data")

cavalli$`type of lesion 3`= NULL  #sistema le colonne e rimuovi quella nulla

temp1<-transform(as.numeric(cavalli$rectal_temperature))
hist(temp1$X_data, freq=F, xlab = "Temperature")#vediamo che c'è una grossa concentrazione di dati sballati
lines(density(temp1$X_data), col = "blue")

#rimuovi gli NA e rendi temperatura numerica

cavalli$`rectal_temperature`<-na.omit(cavalli$`rectal_temperature`)
cavalli$`rectal_temperature`<-as.character(cavalli$`rectal_temperature`)
cavalli$`rectal_temperature`<-as.numeric(cavalli$`rectal_temperature`)
cavalli <- cavalli[!is.na(cavalli$`rectal_temperature`), ]

#inizio funzione rimozione outliers

lowerq = quantile(cavalli$`rectal_temperature`)[2]
upperq = quantile(cavalli$`rectal_temperature`)[4]
iqr = upperq - lowerq

# we identify extreme outliers

extreme.threshold.upper = (iqr * 2) + upperq
extreme.threshold.lower = lowerq - (iqr * 2)

result <- which(cavalli$`rectal_temperature`> extreme.threshold.upper | cavalli$`rectal_temperature` < extreme.threshold.lower)
cavalli<-cavalli[-c(result),]

shapiro.test(cavalli$`rectal_temperature`)# ok pvalue = 0.1387 quindi non possiamo rifiutare H0 (distr normale)

#dopo la trasformazione
hist(cavalli$rectal_temperature, freq = F)#molto meglio
lines(density(cavalli$rectal_temperature), col = "blue")

#fine rimozione outliers, abbiamo il dataset pulito
# Y=rectal_temperature
#rectal_temperature
#- linear
#- in degrees celsius.
#- An elevated temp may occur due to infection.
#- temperature may be reduced when the animal is in late shock
#- normal temp is 37.8
#- this parameter will usually change as the problem progresses
#eg. may start out normal, then become elevated because of
#the lesion, passing back through the normal range as the horse goes into shock

#### PUNTO 1 #### Descriptive analysis on Y (shape, density, statistics), plots ###------------------------------------

hist(cavalli$rectal_temperature, freq = F, xlab = "Temperature")
lines(density(cavalli$rectal_temperature), col = "blue")

#l'istogramma sembra avere il comportamento di una gaussiana con leggera skew verso destra

describe(cavalli$rectal_temperature)

#skew minima poichè = a 0,03
qqnorm(cavalli$`rectal_temperature`, ylab = "Temperature" )# VISUALIZZIAMO LA NORMALITà DELLA NOSTRA DISTRIBUZIONE
qqline(cavalli$`rectal_temperature`)#se una funzione ha una distr normale i punti si distribuiscono sulla riga tracciata questa linea mette in relazione i valori assunti con i quantili

shapiro.test(cavalli$`rectal_temperature`) #shapiro test

#data:  cavalli$`rectal_temperature`
#W = 0.99063, p-value = 0.1387

ks.test(cavalli$rectal_temperature,"pnorm") #kolmogrov smirnof(da valutare se tenere)
#p-value < 2.2e-16
#il test sembra dare valori sballati poichè ci sono valori che si sovrappongono e da problemi

summary(cavalli$`rectal_temperature`) #36.40   37.80   38.20   38.16   38.50   39.90 

#test di jarque bera\ si basa su curtosi e asimmetria che verifichiamo.
#La statistica JB è distribuita asintoticamente come una variabile casuale chi quadro con due gradi di libertà e
#può essere usata per testare l'ipotesi nulla che il campione è stato estratto da una popolazione di dati
#distribuiti come una variabile casuale normale.

sample<-sample2<-sample(cavalli$`rectal_temperature`, size =45)

jarque.bera.test(sample)   #(da valutare se tenere)

#X-squared = 2.5758, df = 2, p-value = 0.2759, quindi anche con questa la variabile risulta normale
#abbiamo il valore X^2 perchè ha una valenza simile ad una chi quadro.

#### PUNTO 2 #### test sulla media.--------------------------------------------------------------------------

#prendiamo il 20% delle osservazioni che sono piu o meno 45 osservazioni  

library ("webr")
library ("moonBook")
library ("stringr")

sample2<-sample(cavalli$`rectal_temperature`, size =45)
t.test(sample2,mu = 38.16)
prova = t.test(sample2,mu = 38.16)
plot(prova)

hist(cavalli$rectal_temperature, xlab = "Temperature" )
abline(v = mean(sample2),col = "royalblue")
abline(v = mean(cavalli$rectal_temperature),col = "red")
#per questo cmapione molto rappresentativo le vedo quasi sovrapposte, ma può cambiare a seconda del campione 

#p-value = 0.7355 questo campione è molto rappresentativo
#IC 38.00671 38.37551


#### PUNTO 3 #### Test two means, two variances (Y vs X binary)-------------------------------------------------
# temp cavalli morti vs quelli vivi
#visualizziamo la differenza tra medie a seconda che sia lesionato o no
boxplot(rectal_temperature ~ surgical_lesion, cavalli, names = c("lesionato","non lesionato"), ylab ="Temperature")
#non si vedono particolari differenze

Sy<-subset(cavalli,cavalli$surgical_lesion==1)
Sn<-subset(cavalli,cavalli$surgical_lesion==2)

m1<-mean(Sy$`rectal_temperature`)
m2<-mean(Sn$`rectal_temperature`)

sd(Sy$`rectal_temperature`)
sd(Sn$`rectal_temperature`)

t.test(Sy$`rectal_temperature`,Sn$`rectal_temperature`, paired=FALSE) # non possiamo rifiutare l'ipotesi nulla pvalue =0.5897

#test sulle varianze

var.test(Sn$`rectal_temperature`,Sy$`rectal_temperature`, alternative = "two.sided") # 0.0074 ipotesi alternativa H1, è che la mdia sia diversa da quella fissata da H0
var.test(Sn$`rectal_temperature`,Sy$`rectal_temperature`, alternative = "less") # 0.9964 quando è minore, H1 = la media è minore rispetto a quella fissata da H0
var.test(Sn$`rectal_temperature`,Sy$`rectal_temperature`, alternative = "greater")# 0.0036 quando la media è maggiore ripetto a quella fissata da H0


#temperatura cavalli vecchi contro quelli giovani
#visualizziamo
boxplot(rectal_temperature ~ age, cavalli, names =c("vecchi", "giovani")) # sembra esserci già più differenza

vecchi<-subset(cavalli,cavalli$age==1)
giovani<-subset(cavalli,cavalli$age==9)

v1<-mean(vecchi$`rectal_temperature`)
g1<-mean(giovani$`rectal_temperature`)

sdv<-sd(vecchi$`rectal_temperature`)
sdg<-sd(giovani$`rectal_temperature`)

t.test(giovani$`rectal_temperature`,vecchi$`rectal_temperature`, paired=FALSE) #rifiutiamo l'ipotesi nulla che le temperature siano uguali pvalue=0.00013

#test sulle varianze

var.test(vecchi$`rectal_temperature`,giovani$`rectal_temperature`, alternative = "two.sided") # 0.21
var.test(vecchi$`rectal_temperature`,giovani$`rectal_temperature`, alternative = "less") # 0.97
var.test(vecchi$`rectal_temperature`,giovani$`rectal_temperature`, alternative = "greater")# 0.10

#### PUNTO 4 ####  Association/chi square among some couples of categorical Xj---------------------------------------------------
#4.a
#cambio a numerico
dummy_cavalli=cavalli

dummy_cavalli[] <- lapply(dummy_cavalli, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
sapply(dummy_cavalli, class) #trasforma i factor in numerici


dummy_cavalli=dummy_cavalli[!rowSums(is.na(dummy_cavalli)) > 3,] #rimuovi le righe con Na>2
corr_matrix=cor(dummy_cavalli)

#4.b
corr<-sapply(cavalli$`capillary_refill_time`, as.numeric)
corr1<-sapply(cavalli$outcome, as.numeric)

cor(corr,corr1, method = c("spearman")) 
cor.test(corr, corr1, method=c("spearman"))#0.2572751 ha un basso valore di correlazione, pvalue 0.00007,
#in questo caso un basso valore del pvalue vuol dire che sono profondamente orrelati
# l'ipotsi nulla è che la correlazione tra le due variabili sia pari a zero

#data:  corr and corr1
#S = 1565800, p-value = 7.094e-05
#alternative hypothesis: true rho is not equal to 0
#sample estimates:
#  rho = 0.2572751

chisq.test(corr,corr1) #test su indipendenza, quindi se è molto basso, vuol dire che sono dipendenti

cor(corr,corr1, method = c("kendall")) #0.2572751 ha un basso valore di correlazione, pval = 0.00009387
cor.test(corr, corr1, method=c("kendall"))

cor(corr,corr1, method = c("pearson")) #0.2572751 ha un basso valore di correlazione, pval = 0.000144
cor.test(corr, corr1, method=c("pearson"))

#correlazione età \ morte ||| dai pvalue si vedono che sono poco correlati |||

età<-sapply(cavalli$age, as.numeric)
morte<-sapply(cavalli$outcome, as.numeric)

cor(età,morte, method = c("spearman")) #0.0689 ha un basso valore di correlazione\ pvalue 0.295
cor.test(età,morte, method=c("spearman"))

cor(età,morte, method = c("kendall")) #0.0662 ha un basso valore di correlazione\ 0.294 
cor.test(età,morte, method=c("kendall"))

cor(età,morte, method = c("pearson")) #0.0329 ha un basso valore di correlazione\ 0.616 
cor.test(età,morte, method=c("pearson"))

chisq.test(età,morte)

#correlazione  capillary_refill_time\packed PER0ò NON è CATEGORIALE E CI DISPIACE :(    

cap_time<-sapply(cavalli$`capillary_refill_time`, as.numeric)
packed<-sapply(cavalli$`packed_cell_volume`, as.numeric)

cor(cap_time,packed, method = c("spearman")) #0.3390 ha un basso valore di correlazione\ pvalue 0.000000112
cor.test(cap_time,packed, method=c("spearman"))

cor(cap_time,packed, method = c("kendall")) #0.2779 ha un basso valore di correlazione\ pvalue 0.000000126
cor.test(cap_time,packed, method=c("kendall"))

cor(cap_time,packed, method = c("pearson")) #0.3432 ha un basso valore di correlazione\ pvalue 0.000000076
cor.test(cap_time,packed, method=c("pearson"))


#correlazione peristalsis \ surgical 

peristalsis<-sapply(cavalli$peristalsis, as.numeric)
surgical<-sapply(cavalli$`surgical_lesion`, as.numeric)

cor(peristalsis,surgical, method = c("spearman")) #-0.2558 ha un basso valore di correlazione\ pvalue 0.000097
cor.test(peristalsis,surgical, method=c("spearman"))

cor(peristalsis,surgical, method = c("kendall")) #-0.2337 ha un basso valore di correlazione\ pvalue 0.00009768
cor.test(peristalsis,surgical, method=c("kendall"))

cor(peristalsis,surgical, method = c("pearson")) #-0.22434 ha un basso valore di correlazione\ pvalue 0.00056
cor.test(peristalsis,surgical, method=c("pearson"))


#IPOTESI DI APPLICAZIONE: covarianze lineari (le vedo dal grafico), e voglio che i dati seguano una distrib normale


#### PUNTO 5 #### ####-----------------------------------------------------------------------------------------
#- Anova one way Y = Xj, for a categorical X

cavalli$`pain`<-na.omit(cavalli$`pain`)
cavalli$`pain`<-as.character(cavalli$`pain`)
cavalli$`pain`<-as.numeric(cavalli$`pain`)
cavalli <- cavalli[!is.na(cavalli$`pain`), ]

#temperatura vs pain
bartlett.test (cavalli$`rectal_temperature`~cavalli$pain) #pvalue dell'omoschedasticità = 0,45
# sono omoschedastiche se hanno tutte la stessa varianza

boxplot(rectal_temperature ~ pain , cavalli, xlab="Pain", ylab ="Temperature", col = "brown") #verifichiamo indipendenza delle osservazioni dalla classe

a1wa<-lm(rectal_temperature ~ pain, data = cavalli)

summary(a1wa)

anova(aov(cavalli$`rectal_temperature` ~ cavalli$pain))# pavalue = 0.5533
#dato il pvalue, la varibile indipendente non produce alcun effetto sulla dipendente (aggiungere commento F value)

#temperatura vs age #a me viene un p-value diverso
bartlett.test (cavalli$`rectal_temperature` ~ cavalli$age)#pvalue dell'omoschedasticità = 0,03 -> accettabile con un confidenza del 1%

boxplot(rectal_temperature ~ age, cavalli, xlab ="età", ylab ="Temperature", col = "pink", names = c("vecchi","giovani"))#verifichiamo indipendenza delle osservazioni dlla classe

a1wb<-lm(rectal_temperature ~ age, data = cavalli)

summary(a1wb)

anova(aov(cavalli$`rectal_temperature` ~ cavalli$age))# pavalue = 0.000277
#dato il pvalue, la varibile indipendente produce un effetto sulla dipendente


#### PUNTO 6 #### ####---------------------------------------------------------------------------------------------

#ANOVA TWO WAY

#Una volta verificato che la popolazione è normale, che i residui sono normali
#e che c'è indipendenza degli errori, allora posso fare Anova a 2 vie

#anova a due vie primo test

cavalli$`pain`<-na.omit(cavalli$`pain`)
cavalli$`pain`<-as.character(cavalli$`pain`)
cavalli$`pain`<-as.numeric(cavalli$`pain`)
cavalli <- cavalli[!is.na(cavalli$`pain`), ]



cavalli$`nasogastric_reflux`<-na.omit(cavalli$`nasogastric_reflux`)
cavalli$`nasogastric_reflux`<-as.character(cavalli$`nasogastric_reflux`)
cavalli$`nasogastric_reflux`<-as.numeric(cavalli$`nasogastric_reflux`)
cavalli <- cavalli[!is.na(cavalli$`nasogastric_reflux`), ]


bartlett.test (cavalli$`rectal_temperature`~cavalli$nasogastric_reflux) #pvalue = 0,9242 #verifico la omoschedasticità 
bartlett.test (cavalli$`rectal_temperature`~cavalli$pain) #pvalue = 0.421

shapiro.test(cavalli$`rectal_temperature`) #verifico la normalità della distribuzione

boxplot(rectal_temperature ~ nasogastric_reflux , cavalli, xlab="Reflux", ylab ="Temperature", col = "yellow") #verifico l'indipendenza
# le medie sono simili, quindi c'è indipendenza rispetto al fatto che ci sia reflusso o meno.

a2w <-aov(cavalli$`rectal_temperature` ~ cavalli$pain + cavalli$nasogastric_reflux) #senza interazioni

a2wwi <- aov(cavalli$`rectal_temperature` ~ cavalli$pain * cavalli$nasogastric_reflux) # se vogliamo vedere l'effetto delle interazioni

anova(a2w)

#Df Sum Sq Mean Sq F value Pr(>F)
#cavalli$pain                 5  1.588 0.31767  0.7950 0.5543
#cavalli$nasogastric_reflux   3  1.041 0.34706  0.8686 0.4581
#Residuals                  224 89.507 0.39959        

anova(a2wwi)

#Df Sum Sq Mean Sq F value Pr(>F)
#cavalli$pain                              5  1.588 0.31767  0.7800 0.5651
#cavalli$nasogastric_reflux                3  1.041 0.34706  0.8521 0.4669
#cavalli$pain:cavalli$nasogastric_reflux  15  4.384 0.29224  0.7175 0.7654
#Residuals                               209 85.124 0.40729 


#VISUALIZZARE INTERAZIONE


cavalli%>%na.omit(.)%>%
  ggplot() +
  aes(x =cavalli$pain, color = cavalli$nasogastric_reflux, group = cavalli$nasogastric_reflux, y =  cavalli$`rectal_temperature`) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")+
  xlab("Pain")+
  ylab("temperature")

cavalli%>%na.omit(.)%>%
  ggplot() +
  aes(x =cavalli$nasogastric_reflux, color = cavalli$pain, group = cavalli$pain, y =  cavalli$`rectal_temperature`) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line")+
  xlab("Reflux")+
  ylab("Temperature")


#### punto 7 #### ####----------------------------------------------------------------------------------------

#ANCOVA
library(coefplot)
library(forestmodel)
library(car)
##


cavalli$pulse<-na.omit(cavalli$pulse)
cavalli$pulse<-as.character(cavalli$pulse)
cavalli$pulse<-as.numeric(cavalli$pulse)
cavalli$pulse<-as.integer(cavalli$pulse)

##
cavalli$`surgery`<-na.omit(cavalli$`surgery`)
cavalli$`surgery`<-as.character(cavalli$`surgery`)
cavalli$`surgery`<-as.numeric(cavalli$`surgery`)
cavalli <- cavalli[!is.na(cavalli$`surgery`), ]


##


cavalli1<-transform(cavalli,rectal_temperature=as.numeric(rectal_temperature))
cavalli<-transform(cavalli,pulse=as.numeric(pulse))
fit=lm(rectal_temperature ~ surgery + pulse + age + temperature.of.extremities + peripheral.pulse + capillary_refill_time + pain + abdominal.distension + outcome + surgical_lesion ,data=cavalli1)
#prova a vedere se riescia rendere i valori del pulse come se fosse singolo e il summary desse l'incremento al variare del valore


summary(fit)

anova(fit)

coefplot(fit, intercept= FALSE)
#A graphical display of the coefficients and standard errors from a fitted model
