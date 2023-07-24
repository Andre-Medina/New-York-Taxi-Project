#█ ▀█ █▄▄▀ █▄▄█ █▄▄�▄▄█ █▄▄�▄▄█
# CONFIGERATION

#where the tallying data goes
tallied_dir = "../../tallied_data/" 

#name of csv file
tally_s2_file = "taxi_data_tallyed_s3.csv"

dfTotal <- read.csv(paste0(tallied_dir, tally_s2_file))       #reads data
dfTotal <- dfTotal[setdiff(colnames(dfTotal),"X")]  #removes index column
dfTotal <- dfTotal[setdiff(colnames(dfTotal),"day_year")]  #removes day column

#borough names in order
boroughs = c("Bronx","Brooklyn","Manhattan","Queens","Staten Island")

#times In Day in of the day in order
timesInDayInDay = c("X2020_night", "X2020_morn", "X2020_arvo", "X2020_even")

#sub title names
subTitle = c("Night", "Morning", "Afternoon", "Evening")

#covid columns which have been selected for the final model
covidCountColumns = c("borough_all_death_count_7day_avg", "borough_case_count_7day_avg")
restrictionColumns = c("stadium_capcity", "indoor_religious")

#prints to check the df looks legit
dfTotal

#factors all the categorical columns
dfTotal$borough <- factor(dfTotal$borough)
dfTotal$fhv <- factor(dfTotal$fhv)
dfTotal$phase_1 <- factor(dfTotal$phase_1)
dfTotal$restaurants <- factor(dfTotal$restaurants)
dfTotal$high_schools <- factor(dfTotal$high_schools)
dfTotal$indoor_religious <- factor(dfTotal$indoor_religious )
dfTotal$day_week <- factor(dfTotal$day_week )



#█ ▀█ █▄▄▀ █▄▄█ █▄▄�▄▄█ █▄▄�▄▄█
#CREATING FINAL MODELS

#makes a blank list
modelList <- list(seq(length(timesInDay)))

#for each part of the day
for(timeIndex in seq(length(timesInDay))) {
  
  #update to console
  print(paste("part of day",timeIndex))
  
  
  #constructs name of the 2019 respective taxi count column
  splitTime <- strsplit(timesInDay[timeIndex], "2020")
  column2019 = paste0(splitTime[[1]][1], "2019", splitTime[[1]][2])
  
  #creates vector of the other features
  otherFeatures = c("day_week", "borough", "week")
  caterFeatures = union(restrictionColumns, c("fhv"))
  continFeatures = union(column2019, covidCountColumns)
  
  
  
  #creates final model formula
  formula = paste0(timesInDay[timeIndex]," ~ +(",
                   do.call(
                     #columns with no interaction
                     paste, c(as.list(otherFeatures), sep = " +")
                   ),") +(",do.call(
                     
                     #interaction 
                     #categorical columns
                     paste, c(as.list(caterFeatures), sep = " +")
                   ),") *(",do.call(
                     #continuous columns
                     paste, c(as.list(continFeatures), sep = " +")
                   ),")"
  )
  
  #adds a fitted to the list
  modelList[[timeIndex]] <- lm( 
    as.formula(formula),
    data = dfTotal)
}


#loops and prints the summary
for(timeIndex in seq(length(timesInDay))) {
  print(summary(modelList[[timeIndex]]))
}

  


#█ ▀█ █▄▄▀ █▄▄█ █▄▄�▄▄█ █▄▄�▄▄█
# GRAPHING FINAL MODELS
#
# graphs the fit of the models fit
graphOfFit <- function(fittedList, columnName, others = c(), sig_level = 0.05) {
  
  #creates an empty array for collating the confidence intervals
  ci_lowers = c()
  ci_uppers = c()
  mids = c()
  
  sig = 0
  count = 0
  
  
  #setting up 2 by 2 plot
  par(mfrow=c(2,2))
  
  #for each part of the day
    for(timeIndex in seq(length(timesInDay))) {
      
      
      #creates a model that has
      #the column of interest
      modelWith <- lm(
        as.formula(
          paste0(timesInDay[timeIndex]," ~ ",
                 do.call(paste, c(as.list(
                   union( #combines old attibutes
                     attr(terms(fittedList[[timeIndex]]), "term.labels"),
                     c(columnName, others))), sep = " + ")  #adds new attibutes
                 )
          )
        ),
        data = dfTotal
      )
      
      
      #creates a model that does not have
      #the column of interest
      modelWout <- lm(
        as.formula(
          paste0(timesInDay[timeIndex]," ~ ",
                 do.call(paste, c(as.list(
                   setdiff( #removes the specified atributes
                     attr(terms(fittedList[[timeIndex]]), "term.labels"),
                     c(columnName, others))), sep = " + ")  #removes new attibutes
                 )
          )
        ),
        data = dfTotal
      )
      
      #prints the comparison of the r^2 values
      print(paste0(timesInDay[timeIndex]," model with week: ",summary(modelWith)$adj.r.squared))
      print(paste0("without week: ", summary(modelWout)$adj.r.squared))
      print(paste0("delta: ", summary(modelWith)$adj.r.squared - summary(modelWout)$adj.r.squared))
      
      #prints CI and sig level
      print(paste0("CI: ", exp(confint(modelWith, "week")[1:2])))
      print(paste0("point: ", exp(modelWith$coefficients["week"][1])))
      print(paste0("sig leve: ", anova(modelWith, modelWout)$`Pr(>F)`[[2]]))
      print("`") #new line
      
      #counts the model if the relavent term was significant
      sig = sig + if (anova(modelWith, modelWout)$`Pr(>F)`[[2]] < sig_level) 1 else 0
      count = count + 1
      
      
      #extracts fitted and real values
      y = as.vector(dfTotal[timesInDay[timeIndex]][[1]])
      yHat = as.vector(modelWith$fitted.values)
      yHat2 = as.vector(modelWout$fitted.values)
      
      #converts fitted to 7 day running average and exponential
      #7days times 10 for each borough taxi/fhv combo = averages of 70 
      y = zoo::rollmean(exp(exp(y)), k = 70) 
      yHat = zoo::rollmean(exp(exp(yHat)), k = 70)
      yHat2 = zoo::rollmean(exp(exp(yHat2)), k = 70)
      
      
      #list of days for graphing
      x = ((35 * 10) : (35 * 10 - 1 + length(yHat))) / 10
      
      #plots values verses fitted
      plot( x, y, type = "l", col = "red", 
            main = sub_title[timeIndex], xlab = "Day of 2020", ylab = "Taxi count", 
            cex.main = 2,cex.axis = 1.4,cex.lab = 1.5)
      lines(x, yHat, col="green", cex = 2)
      lines(x, yHat2, col = "blue", cex = 2)

      #addes a legened to the first plot
      if (timeIndex == 1) {
        legend("top", legend=c(
          "real values",
          paste0("fitted with ",columnName), 
          paste0("fitted without ",columnName)
        ), col = c("red", "green", "blue"), lty = 1:2, cex = 1.5)
        
      }
      
      
    }
  
  #adds a main title to the graph
  mtext("7 day average of the fitted taxi count averaged over boroughs & taxi/ fvh
        vs real values for both models with and with the 'week' term", 
        side = 3, line = -3, outer = TRUE, cex = 1.4)
  
  #returns the significance
  return( list(modelWith, modelWout, sig/ count ))
}


#calls the graphing function
out = graphOfFit(modelList, "week", sig_level = .0001)
print(paste0("of the four models, this percent of them found weeks to be significant: ",out[[3]]*100,"%"))
