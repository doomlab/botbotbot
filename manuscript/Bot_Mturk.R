
library(ez)
library(reshape)
library(ggplot2)
library(moments)
library(cowplot)

##load data
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/bot bot bot/SAD")
master = read.csv("Bot_MTURK.csv")

##exclude no consent
master1 = subset(master, Q1 == 1)

##exclude people who didn't finish
percentmiss = function(x){ sum(is.na(x)) / length(x) * 100}
missing = apply(master1, 1, percentmiss)
table(missing)
master2 = subset(master1, missing < 20)

final = master2[ , -1]

colnames(final) = c("Tfirst", "Tlast", "Tpage", "Click","q1","q2","q3","q4",
                     "q5","q6","q7","q8","q9","q10","q11","q12","q13","q14","q15")

##set up
#remember that zero is OK, one is a problem 
minscale = 1
maxscale = 7
nsim = nrow(final)

####click count check####
totalexp = apply(final[ , 5:19], 1, function(x){sum(!is.na(x))})

for(i in 1:nsim){
  if(final$Click[i] >= totalexp[i]){
    final$badClick[i] = 0
  } else{
    final$badClick[i] = 1
  }
}
table(final$badClick) / nsim*100
mean(final$Click)
sd(final$Click)

####Page submit check####
##http://iovs.arvojournals.org/article.aspx?articleid=2166061
ourchar = 1021
meanchar = 987
sdchar = 118
upperchar = meanchar + 2*sdchar
cutoffChar = ourchar / upperchar * 60
final$badChar = as.numeric(final$Tpage < cutoffChar)
table(final$badChar) / nsim * 100
mean(final$Tpage)
sd(final$Tpage)

####number of scale options used####
OptUse = function(x){
  length(table(as.vector(as.matrix(unname(x)))))
}
numOpt = apply(final[,c(5:18)],1,OptUse)
final$numOpt = as.numeric(numOpt)

for(i in 1:nsim){
  ##more than half of the options
  if(final$numOpt[i] >= 5){
    final$badScaleCheck[i] = 1
  } else{
    final$badScaleCheck[i] = 0
  }
}
table(final$badScaleCheck) / nsim * 100

####Distribution Testing####
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
table(final$dist) / nsim * 100

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
table(final$badDist) / nsim*100

####manipulation check question####
corans = 7

for(i in 1:nsim){
if(final$q15[i] == 7) 
{ final$badMC[i] = 0
  } else { final$badMC[i] = 1} 
}
table(final$badMC) / nsim * 100

####total up####
final$badTotal = as.numeric(final$badChar) + 
  as.numeric(final$badClick) + 
  as.numeric(final$badDist) + 
  as.numeric(final$badScaleCheck) + 
  as.numeric(final$badMC)

table(final$badTotal) / nsim * 100

twoset = subset(final, badTotal == 2)
##figure out how many times this and that

##do they affect things
final$totalRS = apply(final[ , 5:18], 1, sum)
final$condition = as.numeric(final$badTotal >= 2)
good = subset(final, condition == 0)
bad = subset(final, condition == 1)

library(MOTE)

goodvbad = matrix(NA, nrow = 1000, ncol = 150)
colnames(goodvbad) = c("g1", "g2", "g3", "g4", "g5", "g6", "g7", "g8", "g9", "g10", "g11", "g12", "g13", "g14", "gtot", 
             "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b11", "b12", "b13", "b14", "btot", 
             "g1SD", "g2SD", "g3SD", "g4SD", "g5SD", "g6SD", "g7SD", "g8SD", "g9SD", "g10SD", "g11SD", "g12SD", "g13SD", "g14SD", "gtotSD", 
             "b1SD", "b2SD", "b3SD", "b4SD", "b5SD", "b6SD", "b7SD", "b8SD", "b9SD", "b10SD", "b11SD", "b12SD", "b13SD", "b14SD", "btotSD",
             "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d11", "d12", "d13", "d14", "dtot", 
             "dlow1", "dlow2", "dlow3", "dlow4", "dlow5", "dlow6", "dlow7", "dlow8", "dlow9", "dlow10", "dlow11", "dlow12", "dlow13", "dlow14", "dtotlow", 
             "dhigh1", "dhigh2", "dhigh3", "dhigh4", "dhigh5", "dhigh6", "dhigh7", "dhigh8", "dhigh9", "dhigh10", "dhigh11", "dhigh12", "dhigh13", "dhigh14", "dtothigh", 
             "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "ttot", 
             "df1", "df2", "df3", "df4", "df5", "df6", "df7", "df8", "df9", "df10", "df11", "df12", "df13", "df14", "dftot", 
             "p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9", "p10", "p11", "p12", "p13", "p14", "ptot"
             )

for(i in 1:1000){
  
##this part creates the smaller datasets to compare against
##using n = 100 for each group
  good2 = sample(1:nrow(good), 100)
  goodfinal = good[good2 , ]
  
  bad2 = sample(1:nrow(bad), 100)
  badfinal = bad[bad2, ]

Mgood = apply(goodfinal[ , c(5:18, 30)], 2, mean, na.rm = T)
SDgood = apply(goodfinal[ , c(5:18, 30)], 2, sd, na.rm = T)
Ngood = apply(goodfinal[ , c(5:18, 30)], 2, function(x){length(na.omit(x))})

Mbad = apply(badfinal[ , c(5:18, 30)], 2, mean, na.rm = T)
SDbad = apply(badfinal[ , c(5:18, 30)], 2, sd, na.rm = T)
Nbad = apply(badfinal[ , c(5:18, 30)], 2, function(x){length(na.omit(x))})

##loop to figure out how each question is affected
dvalues = 1:15
dlowvalues = 1:15
dhighvalues = 1:15
tvalues = 1:15
dfvalues = 1:15
pvalues = 1:15

for (r in 1:15){
  
  effsize = d.ind.t(Mgood[r], Mbad[r], SDgood[r], SDbad[r], Ngood[r], Nbad[r], a = .05)
  dvalues[r] = effsize$d
  dlowvalues[r] = effsize$dlow
  dhighvalues[r] = effsize$dhigh
  tvalues[r] = effsize$t
  dfvalues[r] = effsize$df
  pvalues[r] = effsize$p
  
} ##end question loop

goodvbad[i, ] = c(Mgood, Mbad, SDgood, SDbad, dvalues, dlowvalues, dhighvalues, tvalues, dfvalues, pvalues)

}##end loop to simulate good versus bad

goodvbad = write.csv(goodvbad, "good_bad_comparison.csv")

goodvbad = read.csv("good_bad_comparison.csv")
goodvbad$averaged = apply(goodvbad[ , 61:76], 1, mean)
goodvbad$averaget = apply(goodvbad[ , 107:121], 1, mean)
goodvbad$averagedf = apply(goodvbad[ , 122:136], 1, mean)
goodvbad$averagep = apply(goodvbad[ , 137:151], 1, mean)
apply(goodvbad[ , 152:155], 2, mean)


##randomly pull groups and see if we get different results
####sensitivity analysis####
proportion = nrow(bad) / nrow(final)
seprop = sqrt(proportion * (1-proportion) / nrow(final))
LLprop = proportion - 1.96*seprop
ULprop = proportion + 1.96*seprop

##loop over sample size 
samplesize = seq(20, 200, 10)
sensematrix = matrix(NA, nrow = 1000*length(samplesize), ncol = 19)
colnames(sensematrix) = c("N", "Mfake1", "Mfake2", "Mfake1w", "Mfake2w", 
                          "SDfake1", "SDfake2", "SDfake1w", "SDfake2w",
                          "d", "dw", "dlow", "dhigh", "dloww", "dhighw",
                          "t", "tw", "p", "pw")
row = 1

for (q in 1:length(samplesize)) { ##loop over the sample size
  
  for (p in 1:1000) { ##do this 1000 times 
    
    randompercent = runif(1, LLprop, ULprop)

    ##fakegroup1 
    good3 = sample(1:nrow(good), round((1-randompercent)*samplesize[q], 0))
    bad3 = sample(1:nrow(bad), round((randompercent)*samplesize[q], 0))
    fakedata1 = rbind(good[good3, ], bad[bad3, ])

    ##fakegroup2
    good4 = sample(1:nrow(good), round((1-randompercent)*samplesize[q], 0))
    bad4 = sample(1:nrow(bad), round((randompercent)*samplesize[q], 0))
    fakedata2 = rbind(good[good4, ], bad[bad4, ])

    withbad = d.ind.t(mean(fakedata1$totalRS, na.rm = T), mean(fakedata2$totalRS, na.rm = T),
                      sd(fakedata1$totalRS, na.rm = T), sd(fakedata2$totalRS, na.rm = T),
                      length(na.omit(fakedata1$totalRS)), length(na.omit(fakedata2$totalRS)), a = .05)
    
    withoutbad = d.ind.t(mean(good[good3, ]$totalRS, na.rm = T), mean(good[good4, ]$totalRS, na.rm = T),
                         sd(good[good3, ]$totalRS, na.rm = T), sd(good[good4, ]$totalRS, na.rm = T),
                         length(na.omit(good[good3, ]$totalRS)), length(na.omit(good[good4, ]$totalRS)), a = .05)
    
    sensematrix[row, ] = c(samplesize[q], withbad$M1, withbad$M2, withoutbad$M1, withoutbad$M2,
                         withbad$sd1, withbad$sd2, withoutbad$sd1, withoutbad$sd2, 
                         withbad$d, withoutbad$d, withbad$dlow, withbad$dhigh, withoutbad$dlow, withoutbad$dhigh,
                         withbad$t, withoutbad$t, withbad$p, withoutbad$p)
    row = row + 1
    
    
  } ##end 1000 loop

} ## end n loop

write.csv(sensematrix, "sensitivity_results.csv")

sense1 = read.csv("sensitivity_results.csv")
sense1$ddiff = abs(sense1$d - sense1$dw)
sense1$pdiff = abs(sense1$p - sense1$pw)
with(sense1, tapply(ddiff, N, mean))
with(sense1, tapply(pdiff, N, mean))


####let's pretend they are different####
##randomly pull groups and see if we get different results
##sensitivity analysis
proportion = nrow(bad) / nrow(final)
seprop = sqrt(proportion * (1-proportion) / nrow(final))
LLprop = proportion - 1.96*seprop
ULprop = proportion + 1.96*seprop

##loop over sample size 
samplesize = seq(20, 200, 10)
sensematrix2 = matrix(NA, nrow = 1000*length(samplesize), ncol = 19)
colnames(sensematrix2) = c("N", "Mfake1", "Mfake2", "Mfake1w", "Mfake2w", 
                          "SDfake1", "SDfake2", "SDfake1w", "SDfake2w",
                          "d", "dw", "dlow", "dhigh", "dloww", "dhighw",
                          "t", "tw", "p", "pw")
row = 1

for (q in 1:length(samplesize)) { ##loop over the sample size
  
  for (p in 1:1000) { ##do this 1000 times 
    
    randompercent = runif(1, LLprop, ULprop)
    
    ##fakegroup1 
    good3 = sample(1:nrow(good), round((1-randompercent)*samplesize[q], 0))
    bad3 = sample(1:nrow(bad), round((randompercent)*samplesize[q], 0))
    tempgood = good[good3, ]
    tempgood$totalRS = tempgood$totalRS + 14
    fakedata1 = rbind(tempgood, bad[bad3, ])
    
    ##fakegroup2
    good4 = sample(1:nrow(good), round((1-randompercent)*samplesize[q], 0))
    bad4 = sample(1:nrow(bad), round((randompercent)*samplesize[q], 0))
    fakedata2 = rbind(good[good4, ], bad[bad4, ])
    
    withbad = d.ind.t(mean(fakedata1$totalRS, na.rm = T), mean(fakedata2$totalRS, na.rm = T),
                      sd(fakedata1$totalRS, na.rm = T), sd(fakedata2$totalRS, na.rm = T),
                      length(na.omit(fakedata1$totalRS)), length(na.omit(fakedata2$totalRS)), a = .05)
    
    withoutbad = d.ind.t(mean(tempgood$totalRS, na.rm = T), mean(good[good4, ]$totalRS, na.rm = T),
                         sd(tempgood$totalRS, na.rm = T), sd(good[good4, ]$totalRS, na.rm = T),
                         length(na.omit(tempgood$totalRS)), length(na.omit(good[good4, ]$totalRS)), a = .05)
    
    sensematrix2[row, ] = c(samplesize[q], withbad$M1, withbad$M2, withoutbad$M1, withoutbad$M2,
                           withbad$sd1, withbad$sd2, withoutbad$sd1, withoutbad$sd2, 
                           withbad$d, withoutbad$d, withbad$dlow, withbad$dhigh, withoutbad$dlow, withoutbad$dhigh,
                           withbad$t, withoutbad$t, withbad$p, withoutbad$p)
    row = row + 1
    
    
  } ##end 1000 loop
  
} ## end n loop

write.csv(sensematrix2, "sensitivity_results2.csv")

sense2 = read.csv("sensitivity_results2.csv")
sense2$ddiff = abs(sense2$d - sense2$dw)
sense2$pdiff = abs(sense2$p - sense2$pw)
with(sense2, tapply(ddiff, N, mean))
with(sense2, tapply(pdiff, N, mean))
