

## requires the 'moments' package
##      install.packages("moments")
## args: Dataframe of scale values, with each row being a participant and each column being a scale question
## args: vector of response time/page time for each participant
## args: range of scale (e.g. 1-7 Likert scale would be 7)
## args: partno: vector of corresponding participant numbers
## args: clicks: click count column, outputted by qualtrics
## returns: table of identified cases, where True equals an identified automated response
## creates a vector "identified" of True and False, where True identifies specific cases to note 
##     specific participants
## creates a subsetted dataframe "subdata" excluding identified cases


SAD = function(dat,rt,range,partno,click){
  
  #skewness/kurtosis check
  library(moments)
  skewstuff = apply(dat, 1, skewness)
  skewstuff[is.na(skewstuff)] <- 0
  kurstuff = apply(dat,1,kurtosis)
  kurstuff[is.na(kurstuff)] <- 0
  cutoffs = mean(skewstuff, na.rm = T) + 2 * sd(skewstuff, na.rm = T)
  cutoffk = mean(kurstuff, na.rm = T) + 2 * sd(kurstuff, na.rm = T)
  total = as.numeric(abs(skewstuff) < cutoffs)
  total2 = as.numeric(abs(kurstuff) < cutoffk)
  
  #number of scale options used
  OptUse = function(x){
    length(table(as.vector(as.matrix(unname(x)))))
  }
  numOpt = apply(dat,1,OptUse)
  if(range <= 5){
    cutoffOpt = range  - 1
    
  } else {
    cutoffOpt = range - 2
  }
  total3 = as.numeric(abs(numOpt) >= cutoffOpt)
  
  #response/page time
  cutoffTime = unname(quantile(rt)[2])
  total4 = as.numeric(rt < cutoffTime)
  
  #click counts
  clix = click
  clix = replace(clix,clix==0,1)
  clix = replace(clix,clix!=1,0)
  total5 = clix
  
  #if participant meets all 4 criteria
  totalend = total+total2+total3+total4+total5
  identified <- totalend == 5
  dat = cbind(dat,partno)
  subdata <- subset(dat, totalend != 5)
  alldata <- cbind(dat, 
                   "skewprobs" = as.logical(total),
                   "kurtprobs" = as.logical(total2),
                   "rangeprobs" = as.logical(total3),
                   "timeprobs" = as.logical(total4))
  return(list( "table" = table(totalend == 5),
               "identified" = identified,
               "alldata" = alldata,
               "subdata" = subdata) )
}

## example
data <- read.csv("C:/Users/John/Desktop/SADexampleData.csv")

data$participant = 1:nrow(data)
output = SAD(dat = data[,2:16], rt = data$Duration, range = 7, partno = data$participant, click = data$Q2_Click.Count)

output$table ##total number of suspect data points

output$identified ##logical to show which data points are bad

View(output$subdata) ##dataframe without bad data points 

View(output$alldata) ##dataframe with indicators of bad data points


##stuff
temprow = as.numeric(unname(data[1,2:16 ])) ##change this thing here to be selecting one row at a time

minscale = 1
maxscale = 5
utable = matrix(0, nrow = 1, ncol = length(minscale:maxscale))
for(i in minscale:maxscale) {
  utable[i] = length(temprow[ temprow == i])
}
uniformest = chisq.test(utable,
                        rescale.p = T,
                        simulate.p.value = T)


##test normal distribution
##first convert to z score
ztest = scale(temprow)
##then figure out how much of the data is binned for SDs
ztable = matrix(0, nrow = 1, ncol = 6)
colnames(ztable) = c("a", "b", "c", "d", "e", "f")
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
uniformest$statistic
normalest$statistic
