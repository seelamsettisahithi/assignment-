### Assignment-2 ###

### Question-1 ###

Cutlets<-read.csv(file.choose())
attach(Cutlets)
View(Cutlets)
colnames(Cutlets)<- c("unit_A","unit_B")

attach(Cutlets)
View(Cutlets)

## applying shaprio test ##
shapiro.test(unit_A)
shapiro.test(unit_B)

var.test(unit_A,unit_B)

#### sample T Test ####
t.test(unit_A,unit_B,alternative = "two.sided",conf.level = 0.95,correct = TRUE)
?t.test
t.test(unit_A,unit_B,alternative = "greater")


### one way ANOVA  ###
Cutlets1<-read.csv(file.choose())
attach(Cutlets1)
View(Cutlets1)
stack_Cutlets1 <-stack(Cutlets1)
attach(stack_Cutlets1)
anova_results <-aov(values~ind,data = stack_Cutlets1)
summary(anova_results)

### Question -4 ###

### customer order form  ###
cof<-read.csv(file.choose())
View(cof)
attach(cof)
t2<-prop.table(table(Phillippines))
t3<-prop.table(table(Indonesia))
t4<-prop.table(table(Malta))
t5<-prop.table(table(India))


### chi square test ###

chisq.test(table(Phillippines))
chisq.test(table(Indonesia))
chisq.test(table(Malta))
chisq.test(table(India))


?prop.test
prop.test(x=c(58,152),n=c(480,740),conf.level = 0.95,correct = FALSE,alternative = "two.sided")

### un equal proportion ###
prop.test(x=c(58,152),n=c(480,740),conf.level = 0.95,correct = FALSE,alternative = "less")

### Question -3 ###

### buyer ratio ###

### chi-square test###
BRO <-read.csv(file.choose())
View(BRO)
attach(BRO)

t2<-prop.table(table(East))
t3<-prop.table(table(West))
t4<-prop.table(table(North))
t5<-prop.table(table(South))
t1<-table(Observed.Values)



chisq.test(table(Observed.Values,East))
chisq.test(table(Observed.Values,West))
chisq.test(table(Observed.Values,North))
chisq.test(table(Observed.Values,South))

### Question-4 ###

### lab TAT ###
labTAT<-read.csv(file.choose())
View(labTAT)
attach(labTAT)

### one way Anova ###

labtat1<-read.csv(file.choose())
View(labtat1)
attach(labtat1)
stack_labtat1 <-stack(labtat1)
attach(stack_labtat1)
anova_results <- aov(values~ind,data = stack_labtat1)
summary(anova_results)

table5<-table(Laboratory.1,Laboratory.2,Laboratory.3,Laboratory.4)
table5

prop.test(x=c(58,152),n=c(480,740),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
prop.test(x=c(58,152),n=c(480,740),conf.level = 0.95,correct = FALSE,alternative = "less")
