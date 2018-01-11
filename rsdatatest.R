rsdata <- read.csv("~/OneDrive - Missouri State University/RESEARCH/2 projects/IRT/data/Meaning_Scales_RS14_RR_RN_Paper_Fixed.csv")

library(moments)
skewstuff = apply(rsdata[,3:16], 1, skewness)
kurstuff = apply(rsdata[ ,3:16], 1, kurtosis)

mean(skewstuff, na.rm = T)
mean(kurstuff, na.rm = T)

##create cutoffs 
cutoff = .008 + 2 * .14
cutoffk = 1.77 + 2 * .11

table(abs(skewstuff) < cutoff)
table(abs(kurstuff) < cutoffk)
table(abs(kurstuff) < cutoffk,abs(skewstuff) < cutoff)

total = as.numeric(abs(skewstuff) < cutoff)
total = total + as.numeric(abs(kurstuff) < cutoffk)

badpeeps = subset(rsdata, total == 2)

##calculate skew and kurtosis by number of questions and number of people
##also include min and max values
##based on 1000/1000
rsdata = na.omit(rsdata)
sk = 1:nrow(rsdata)
kt = 1:nrow(rsdata)
round = 1
for (i in 1:nrow(rsdata))
  {
    temp = runif(14, min = 1, max = 7)
    sk[round] = skewness(temp)
    kt[round] = kurtosis(temp)
    round = round + 1
  }

sims = 1000
items = 1000
sk = 1:sims
kt = 1:sims
round = 1
for (i in 1:sims)
{
  temp = runif(items, min = 1, max = 7)
  sk[round] = skewness(temp)
  kt[round] = kurtosis(temp)
  round = round + 1
}

mean(sk); sd(sk)
mean(kt); sd(kt)

cutoffs = mean(sk) + 2 * sd(sk)
cutoffk = mean(kt) + 2 * sd(kt)

realskew = apply(rsdata[ , 3:16], 1, skewness)
realkurt = apply(rsdata[ , 3:16], 1, kurtosis)

table(abs(realskew) < cutoffs)
table(abs(realkurt) < cutoffk)
table(abs(realkurt) < cutoffk, abs(realskew) < cutoffs)

skewbad = as.numeric(abs(realskew) < cutoffs)
skewbad[is.na(skewbad)] = 0
kurtbad = as.numeric(abs(realkurt) < cutoffk)
kurtbad[is.na(kurtbad)] = 0

total = skewbad + kurtbad
badpeeps = subset(rsdata, total == 2)
apply(badpeeps[ , 3:16], 1, table)

##look at the number of items maybe if you are using majority of items
