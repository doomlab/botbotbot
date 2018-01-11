library(MOTE)
library(ez)
library(reshape)
library(ggplot2)
library(moments)
library(cowplot)

##load data
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/bot bot bot/manuscript")
master = read.csv("Bot_MSU.csv")

##exclude no consent
master1 = subset(master, Q1 == 1)

##exclude people who didn't finish
percentmiss = function(x){ sum(is.na(x)) / length(x) * 100}
missing = apply(master1, 1, percentmiss)
table(missing)
master2 = subset(master1, missing == 0)

##exclude people who got the check questions wrong
#master2$first = master2$Q7 == 1
#master2$second = master2$Q10 == 3
#master2$third = master2$Q14 == 2
#with(master2, table(first))
#with(master2, table(second))
#with(master2, table(third))

master3 = subset(master2, master2$Q7 == 1)
master3 = subset(master3, master3$Q10 == 3)
master3 = subset(master3, master3$Q14 == 2)

##exclude people who used the bot on the wrong page
summary(master3$Q5_4)
summary(master3$Q7_4)
summary(master3$Q11_4)

master4 = subset(master3, Q7_4 >= 15)
master4 = subset(master4, Q11_4 < 15)

##restructure the data
realdat = master4[,c(2:20)]
realdat$condition = "real"
colnames(realdat) = c("Tfirst", "Tlast", "Tpage", "Click","q1","q2","q3","q4",
                      "q5","q6","q7","q8","q9","q10","q11","q12","q13","q14","q15", "condition")
realdat$partno = 1:nrow(realdat)

randdat = master4[,c(22:40)]
randdat$condition = "rand"
colnames(randdat) = c("Tfirst", "Tlast", "Tpage", "Click","q1","q2","q3","q4",
                      "q5","q6","q7","q8","q9","q10","q11","q12","q13","q14","q15","condition")
randdat$partno = 1:nrow(randdat)

botdat = master4[,c(42:60)]
botdat$condition = "bot"
colnames(botdat) = c("Tfirst", "Tlast", "Tpage", "Click","q1","q2","q3","q4",
                      "q5","q6","q7","q8","q9","q10","q11","q12","q13","q14","q15","condition")
botdat$partno = 1:nrow(botdat)

final = rbind(realdat, randdat, botdat)
final$condition = factor(final$condition)

##set up for below
nsim = nrow(final)

####click count####
##remember to include the manip check 
totalexp = apply(final[ , 5:19],1, function(x){sum(!is.na(x))})

for(i in 1:nsim){
  if(final$Click[i] >= totalexp[i]){
    final$badClick[i] = 0
  } else{
    final$badClick[i] = 1
  }
}
table(final$badClick, final$condition)

##anova of differences
ezANOVA(data = final,
        wid = partno,
        within = condition,
        dv = Click,
        type = 3)
##get means, sd, length
Mclick = tapply(final$Click, list(final$condition), mean)
SDclick = tapply(final$Click, list(final$condition), sd)
Nclick = tapply(final$Click, list(final$condition), length)
##post hoc
pairwise.t.test(final$Click, final$condition, 
                paired = T,
                p.adjust.method = "bonferroni")

#bot rand
d.dep.t.avg(Mclick[1], Mclick[2],
            SDclick[1], SDclick[2],
            Nclick[1], a = .05)

#bot real
d.dep.t.avg(Mclick[1], Mclick[3],
            SDclick[1], SDclick[3],
            Nclick[1], a = .05)

#real rand
d.dep.t.avg(Mclick[3], Mclick[2],
            SDclick[3], SDclick[2],
            Nclick[3], a = .05)

####page submit response time####
#remember that zero is OK, one is a problem 
##http://iovs.arvojournals.org/article.aspx?articleid=2166061
ourchar = 1021
meanchar = 987
sdchar = 118
upperchar = meanchar + 2*sdchar
cutoffChar = ourchar / upperchar * 60
final$badChar = as.numeric(final$Tpage < cutoffChar)
table(final$badChar, final$condition)/94*100

ezANOVA(data = final,
        wid = partno,
        within = condition,
        dv = Tpage,
        type = 3)
##get means, sd, length
Mpage = tapply(final$Tpage, list(final$condition), mean)
SDpage = tapply(final$Tpage, list(final$condition), sd)
Npage = tapply(final$Tpage, list(final$condition), length)
##post hoc
pairwise.t.test(final$Tpage, final$condition, 
                paired = T,
                p.adjust.method = "bonferroni")

#bot rand
d.dep.t.avg(Mpage[1], Mpage[2],
            SDpage[1], SDpage[2],
            Npage[1], a = .05)

#bot real
d.dep.t.avg(Mpage[1], Mpage[3],
            SDpage[1], SDpage[3],
            Npage[1], a = .05)

#real rand
d.dep.t.avg(Mpage[3], Mpage[2],
            SDpage[3], SDpage[2],
            Npage[3], a = .05)

####Skewness####
skewstuff = apply(final[,c(5:18)], 1, skewness)
final$skew = as.numeric(skewstuff)
summary(final$skew)
final$skew[is.na(final$skew)] <- 0

ezANOVA(data = final,
        wid = partno,
        within = condition,
        dv = skew,
        type = 3)
##get means, sd, length
tapply(final$skew, list(final$condition), mean)
tapply(final$skew, list(final$condition), sd)
tapply(final$skew, list(final$condition), length)
##post hoc
pairwise.t.test(final$skew, final$condition, 
                paired = T,
                p.adjust.method = "bonferroni")

####Kurtosis####
kurstuff = apply(final[,c(5:18)], 1, kurtosis)
final$kurt = as.numeric(kurstuff)
summary(final$kurt)
final$kurt[is.na(final$kurt)] <- 0

ezANOVA(data = final,
        wid = partno,
        within = condition,
        dv = kurt,
        type = 3)
##get means, sd, length
tapply(final$kurt, list(final$condition), mean)
tapply(final$kurt, list(final$condition), sd)
tapply(final$kurt, list(final$condition), length)
##post hoc
pairwise.t.test(final$kurt, final$condition, 
                paired = T,
                p.adjust.method = "bonferroni")

####number of scale options used####
OptUse = function(x){
  length(table(as.vector(as.matrix(unname(x)))))
}
numOpt = apply(final[,c(5:18)],1,OptUse)
final$numOpt = as.numeric(numOpt)

ezANOVA(data = final,
        wid = partno,
        within = condition,
        dv = numOpt,
        type = 3)
##get means, sd, length
Mpoint = tapply(final$numOpt, list(final$condition), mean)
SDpoint = tapply(final$numOpt, list(final$condition), sd)
Npoint = tapply(final$numOpt, list(final$condition), length)
##post hoc
pairwise.t.test(final$numOpt, final$condition, 
                paired = T,
                p.adjust.method = "bonferroni")

#bot rand
d.dep.t.avg(Mpoint[1], Mpoint[2],
            SDpoint[1], SDpoint[2],
            Npoint[1], a = .05)

#bot real
d.dep.t.avg(Mpoint[1], Mpoint[3],
            SDpoint[1], SDpoint[3],
            Npoint[1], a = .05)

#real rand
d.dep.t.avg(Mpoint[3], Mpoint[2],
            SDpoint[3], SDpoint[2],
            Npoint[3], a = .05)


final$badScaleCheck = "NA"
nsim = nrow(final)
for(i in 1:nsim){
  ##more than half of the options
  if(final$numOpt[i] >= 5){
    final$badScaleCheck[i] = 1
  } else{
    final$badScaleCheck[i] = 0
  }
}
table(final$badScaleCheck, final$condition)/94*100

####Distribution Testing####
minscale = 1
maxscale = 7

for(i in 1:nsim){
  temprow = as.numeric(unname(final[i,5:18 ])) 
  utable = matrix(0, nrow = 1, ncol = length(minscale:maxscale))
  for(x in minscale:maxscale) {
    utable[x] = length(temprow[ temprow == x])
  }
  uniformest = chisq.test(utable,
                          rescale.p = T,
                          simulate.p.value = T)
  
  ##test normal distribution
  ##first convert to z score
  ztest = scale(temprow)
  ##then figure out how much of the data is binned for SDs
  ztable = matrix(0, nrow = 1, ncol = 6)
  ztable[1] = length(ztest[ ztest <= -2 ])  
  ztable[2] = length(ztest[ ztest > -2 & ztest <= -1  ])
  ztable[3] = length(ztest[ ztest > -1 & ztest <= 0 ])
  ztable[4] = length(ztest[ ztest > 0 & ztest <= 1 ])
  ztable[5] = length(ztest[ ztest > 1 & ztest <= 2 ])
  ztable[6] = length(ztest[ ztest > 2 ])
  znormal = c(0.0228, 0.1359, 0.3413, 0.3413, 0.1359, 0.0228)
  normalest = chisq.test(ztable,
                         p = znormal*sum(ztable),
                         rescale.p = T,
                         simulate.p.value = T)
  ##output return chi square values
  final$uniform[i] = uniformest$statistic
  final$normal[i] = normalest$statistic
  
  ##if uniform < normal
  if(uniformest$statistic < normalest$statistic)
  {
    ##if more than 1 option
    if (final$numOpt[i]>1)
      {
      final$dist[i] = 0 ##uniform is better
    } else { ##handles when people only pick one thing
        final$dist[i] = 2
      }
    
  }
  
  ##if uniform >= normal
  if(uniformest$statistic >= normalest$statistic)
  {
    ##if more than 1 option
    if (final$numOpt[i]>1)
    {
      final$dist[i] = 1 ##normal is better
    } else { ##handles when people only pick one thing
      final$dist[i] = 2
    }
    
  }
    

  #0 means uniform fits better
  #1 means normal fits better
  #2 means only chose one scale option

  }## end of for loop

final$dist = factor(final$dist,
                    levels = 0:2,
                    labels = c("uniform", "normal", "undecided"))
table(final$dist, final$condition)

for(i in 1:nsim){
  if(final$dist[i] == "uniform"){
    final$badDist[i] = 1
  }
  if (final$dist[i] == "normal"){
    final$badDist[i] = 0
  } else if(final$dist[i] == "undecided") {
    final$badDist[i] = 0
  }
}
table(final$badDist, final$condition) / 94 * 100 

####manipulation check question####
table(final$q15, final$condition)

corans = 7

for(i in 1:nsim){
  if(final$q15[i] == 7) 
  { final$badMC[i] = 0
  } else { final$badMC[i] = 1} 
}
table(final$badMC, final$condition) / 94 * 100

###lunch happened here :)

####total up####
final$badTotal = as.numeric(final$badChar) + 
  as.numeric(final$badClick) + 
  as.numeric(final$badDist) + 
  as.numeric(final$badScaleCheck) + 
  as.numeric(final$badMC)

table(final$badTotal, final$condition) / 94 * 100

########## Graphs ####
theme = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line.x = element_line(colour = "black"), 
              axis.line.y = element_line(colour = "black"),
              legend.key = element_rect(fill = "white"),
              text = element_text(size = 15))

#page submit response time
bargraph = ggplot(final, aes(condition, Tpage)) +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               fill = "White", 
               color = "Black") +
  stat_summary(fun.data = mean_cl_normal,                
               geom = "errorbar", 
               position = position_dodge(width = 0.90),
               width = 0.2) +
  theme +
  xlab("Condition") +
  ylab("Response Time") +
  scale_x_discrete(labels = c("Automated", "Low Effort", "High Effort"))

#Click Count
bargraph2 = ggplot(final, aes(condition, Click)) +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               fill = "White", 
               color = "Black") +
  stat_summary(fun.data = mean_cl_normal,                
               geom = "errorbar", 
               position = position_dodge(width = 0.90),
               width = 0.2) +
  theme +
  xlab("Condition") +
  ylab("Click Count") +
  scale_x_discrete(labels = c("Automated", "Low Effort", "High Effort"))

#Skewness
bargraph3 = ggplot(final, aes(condition, skew)) +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               fill = "White", 
               color = "Black") +
  stat_summary(fun.data = mean_cl_normal,                
               geom = "errorbar", 
               position = position_dodge(width = 0.90),
               width = 0.2) +
  theme +
  xlab("Condition") +
  ylab("Skewness") +
  scale_x_discrete(labels = c("Automated", "Low Effort", "High Effort"))

#Kurtosis
bargraph4 = ggplot(final, aes(condition, kurt)) +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               fill = "White", 
               color = "Black") +
  stat_summary(fun.data = mean_cl_normal,                
               geom = "errorbar", 
               position = position_dodge(width = 0.90),
               width = 0.2) +
  theme +
  xlab("Condition") +
  ylab("Kurtosis") +
  scale_x_discrete(labels = c("Automated", "Low Effort", "High Effort"))

#Number of scale options used
bargraph5 = ggplot(final, aes(condition, numOpt)) +
  stat_summary(fun.y = mean, 
               geom = "bar", 
               fill = "White", 
               color = "Black") +
  stat_summary(fun.data = mean_cl_normal,                
               geom = "errorbar", 
               position = position_dodge(width = 0.90),
               width = 0.2) +
  theme +
  xlab("Condition") +
  ylab("Number of Scale Options Used") +
  scale_x_discrete(labels = c("Automated", "Low Effort", "High Effort"))

plot_grid( bargraph,
           bargraph2,
           bargraph3,
           bargraph4,
           bargraph5,
           hjust = -1,
           nrow = 3
)




