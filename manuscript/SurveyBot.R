
library(reshape)
library(moments)

## Load Survey Bot data
#SurveyBot <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/SurveyBot/SurveyBot1k.csv")
SurveyBot = read.csv("SurveyBot1k.csv")
SurveyBot[] <- lapply(SurveyBot, function(x) {
  if(is.integer(x)) as.numeric(as.character(x)) else x
})
SurveyBot$Partno = 1:nrow(SurveyBot)

######missing data
##several did not record, thus we ran round two to get 1000 responses
percentmiss = function(x){sum(is.na(x)/length(x))*100}
##rows
missing = apply(SurveyBot,1,percentmiss)
table(missing)
nomiss = subset(SurveyBot, missing<=5)

#add5 <- read.csv("~/John M.S. Experimental Psychology/Research and Articles/Statistics/SurveyBot/SurveyBot5.csv")
add5 = read.csv("SurveyBot5.csv")
add5[] <- lapply(add5, function(x) {
  if(is.integer(x)) as.numeric(as.character(x)) else x
})
add5$Partno = nrow(SurveyBot)+1:nrow(add5) + nrow(SurveyBot)
##rows
missing2 = apply(add5,1,percentmiss)
table(missing2)

fullsurvey = rbind(nomiss, add5)

####click count####
summary(fullsurvey$ClickCount)

####page timing####
summary(fullsurvey$FirstClick)
summary(fullsurvey$LastClick)
summary(fullsurvey$PageSubmit)
sd(fullsurvey$PageSubmit)
#quantiles of page submit
quantile(fullsurvey$PageSubmit)


##create melted data
longdata = melt(fullsurvey,
                id = c("Partno","IP","Duration",
                       "FirstClick","LastClick",
                       "PageSubmit","ClickCount"))

##look at the distribution of the bot data
hist(longdata$value, breaks = 22)
table(longdata$value)

##make histogram
library(ggplot2)
cleanup = theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(), 
                axis.line.x = element_line(color = "black"),
                axis.line.y = element_line(color = "black"),
                legend.key = element_rect(fill = "white"),
                text = element_text(size = 15))

ggplot(longdata, aes(value)) +
  geom_histogram(binwidth = .4, 
                 color = "black", 
                 fill = "white") +
  xlab("Answer Choices") +
  ylab("Frequency") +
  cleanup 

####skew and kurtosis####
pno = c(1:1000)
sk = c(1:1000)
ku = c(1:1000)
round = 0

for(i in 1:nrow(fullsurvey)){
  round = round+1
  x = unname(unlist(fullsurvey[round, -c(1:6,107)]))
  sk[round] = skewness(x)
  ku[round] = kurtosis(x)
}

options(scipen = 999)
skewkurt = as.data.frame(cbind(pno,sk,ku))

## average skewness
mean(skewkurt$sk)
sd(skewkurt$sk)

## average kurtosis
mean(skewkurt$ku, na.rm = T)
sd(skewkurt$ku)


