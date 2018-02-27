## args: dat: Dataframe of scale values, with each row being a participant and each column being a scale question
## args: rt: vector of response time/page time for each participant
## args: min: minimum value of scale
## args: max: maximum value of scale
## args: partno: vector of corresponding participant numbers
## args: clicks: click count column, outputted by qualtrics
## args: manvec: vector of manipulation check question responses
## args: mancor: correct answer of manipulation check
## args: char: number of characters on the page

SAD = function(dat, #data frame of only scale values
               rt = NULL, #column name for page timing
               min = 1, #lower end of scale points
               max = 7, #upper end of scale points
               partno, #participant number so you can merge and identify outliers
               click = NULL, #column of click counts
               manvec = NULL, #column of manipulation check
               mancor = NULL, #answer to the manipulation check
               char = NULL){ #number of characters on the page
  
  ##make sure the data provided is a data frame
  dat = as.data.frame(dat)
  
  ####number of scale options used####
  OptUse = function(x){
    length(table(as.vector(as.matrix(unname(x)))))
  }
  numOpt = apply(dat,1,OptUse)
  numOpt = as.numeric(numOpt)
  nsim = length(numOpt)
  badScaleCheck = rep(NA, length(numOpt))
  for(i in 1:nsim){
    ##more than half of the options
    optionhalf = length(min:max)/2+1
    if(numOpt[i] >= optionhalf){
      badScaleCheck[i] = 1
    } else { badScaleCheck[i] = 0 }
  }
  badScaleCheck = as.numeric(badScaleCheck)
  
  
  ####response/page time####
  if (!is.null(rt)){
  rt = as.numeric(unlist(rt))
  
  ourchar = char
  meanchar = 987
  sdchar = 118
  upperchar = meanchar + 2*sdchar
  cutoffChar = ourchar / upperchar * 60
  badChar = rep(NA, length(rt))
  nsim = length(badChar)
  for(i in 1:nsim){
    if(!is.na(rt[i])){
    if(rt[i] < cutoffChar){
      badChar[i] = 1
    } else{
      badChar[i] = 0
    }
    } else { badChar[i] = NA}
  }
  badChar = as.numeric(badChar)
  } else { badChar = rep(NA, nrow(dat))}
  
  ####click count check####
  if(!is.null(click)){
  click = as.numeric(unlist(click))
  totalexp = apply(dat,1, function(x){sum(!is.na(x))})
  nsim = length(click)
  badClick = rep(NA, length(click))
  for(i in 1:nsim){
    if(!is.na(click[i])){
    if(click[i] >= totalexp[i]){
      badClick[i] = 0
    } else{
      badClick[i] = 1
    }
    } else{ badClick[i] = NA}
  }
  badClick = as.numeric(badClick)
  } else { badClick = rep(NA, nrow(dat))}
  
  ####manipulation check question manip####
  if(!is.null(manvec)){
  manvec = as.numeric(unlist(manvec))
  nsim = length(manvec)
  badMC = rep(NA, nrow(dat))
  for(i in 1:nsim){
    if(!is.na(manvec[i])){ 
      if(manvec[i] == mancor) { badMC[i] = 0 } else { badMC[i] = 1}} else { badMC[i] = 1}
  }
  badMC = as.numeric(badMC)
  } else { badMC = rep(NA, nrow(dat))}
  
  ####Distribution Testing####
  uniform = rep(NA,nrow(dat))
  normal = rep(NA, nrow(dat))
  dist = rep(NA, nrow(dat))
  nsim = nrow(dat)
  for(i in 1:nsim){
    temprow = as.numeric(unname(dat[i,])) 
    utable = matrix(0, nrow = 1, ncol = length(min:max))
    for(x in min:max) {
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
    uniform[i] = uniformest$statistic
    normal[i] = normalest$statistic
    ##if uniform < normal
    if(uniformest$statistic < normalest$statistic)
    {
      ##if more than 1 option
      if (numOpt[i]>1)
      {
        dist[i] = 0 ##uniform is better
      } else { ##handles when people only pick one thing
        dist[i] = 2
      }
    }
    ##if uniform >= normal
    if(uniformest$statistic >= normalest$statistic)
    {
      ##if more than 1 option
      if (numOpt[i]>1)
      {
        dist[i] = 1 ##normal is better
      } else { ##handles when people only pick one thing
        dist[i] = 2
      }
      
    }
    #0 means uniform fits better
    #1 means normal fits better
    #2 means only chose one scale option
  }## end of for loop
  
  ####distribution coding####
  badDist = rep(NA, nsim)
  for(i in 1:nsim){
    if(dist[i] == 0){
      badDist[i] = 1
    } 
    if (dist[i] == 1){
      badDist[i] = 0
    } 
    if (dist[i] == 2) {
      badDist[i] = 0
    }
  }
  badDist = as.numeric(badDist)
  
  ####total up####
  badDF = cbind.data.frame(badChar, badClick, badDist, badScaleCheck, badMC)
  badDF$badTotal = apply(badDF, 1, sum, na.rm = T)
  badDF$participant = partno
  
  return(badDF)
  
} #end function
#########################################################################################
#########################################################################################

library(readxl)
setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/David thesis/OSF")
master <- as.data.frame(read_excel("Thesis_Data.xlsx"))
master$partno = 1:nrow(master)
master = master[1:10, ]
names(master)

LGSbad = SAD(dat = master[ , c(13:32)], 
    rt = master$LGS_Time,
    min = 1,
    max = 4, 
    partno = master$partno,
    click = master$LGS_Click_Count,
    manvec = NULL,
    mancor = NULL,
    char = 1414
    )

MLQbad = SAD(dat = master[ , c(46:55)], 
             rt = master$MLQ_Time,
             min = 1,
             max = 7, 
             partno = master$partno,
             click = master$MLQ_Click_Count,
             manvec = master$MLQ_1,
             mancor = 4,
             char = 841
)
