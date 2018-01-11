sense1 = read.csv("sensitivity_results.csv")
sense1$ddiff = abs(sense1$d - sense1$dw)
sense1$tdiff = abs(sense1$t - sense1$tw)
sense1$pdiff = abs(sense1$p - sense1$pw)
Nddiff = with(sense1, tapply(ddiff, N, mean))
Npdiff = with(sense1, tapply(pdiff, N, mean))
Ntdiff = with(sense1, tapply(tdiff, N, mean))

sense2 = read.csv("sensitivity_results2.csv")
sense2$ddiff = abs(sense2$d - sense2$dw)
sense2$tdiff = abs(sense2$t - sense2$tw)
sense2$pdiff = abs(sense2$p - sense2$pw)
Rddiff = with(sense2, tapply(ddiff, N, mean))
Rpdiff = with(sense2, tapply(pdiff, N, mean))
Rtdiff = with(sense2, tapply(tdiff, N, mean))

for (q in 1:length(Npdiff)){
  Npdiff[q] = p.value(as.numeric(Npdiff[q]))
  Rpdiff[q] = p.value(as.numeric(Rpdiff[q]))
}

tableprint4 = matrix(NA, nrow = 19, ncol = 7)
colnames(tableprint4) = c("Sample Size", "Null $d$", "$t$", "$p$", 
                          "Alt $d$", "$t$", "$p$")

tableprint4[ , 1] = names(Nddiff)
tableprint4[ , 2] = apa(Nddiff,2)
tableprint4[ , 3] = apa(Ntdiff,2)
tableprint4[ , 4] = Npdiff
tableprint4[ , 5] = apa(Rddiff,2)
tableprint4[ , 6] = apa(Rtdiff,2)
tableprint4[ , 7] = Rpdiff

apa_table(tableprint4,
          align = c("l", rep("c", 6)),
          caption = "Sensitivity analysis for null and alternative hypotheses",
          note = "All values are difference scores where summary statistics with flagged data 
          were subtracted from summary statistics without flagged data. These values were 
          averaged over 1000 interations for each group sample size."
)