#█ ▀█ █▄▄▀ █▄▄█ █▄▄�▄▄█ █▄▄�▄▄█
# CONFGURATION
#
#where the tallying data goes
tallied_dir = "../../tallied_data/" 

#name of csv file
tally_s2_file = "taxi_data_tallyed_s3.csv"

dfTotal <- read.csv(paste0(tallied_dir, tally_s2_file))       #reads data
dfTotal <- dfTotal[setdiff(colnames(dfTotal),"X")]  #removes index column
dfTotal <- dfTotal[setdiff(colnames(dfTotal),"day_year")]  #removes day column

#borough names in order
boroughs = c("Bronx","Brooklyn","Manhattan","Queens","Staten Island")

#times of the day in order
timesInDay = c("X2020_night", "X2020_morn", "X2020_arvo", "X2020_even")

#sub title names
subTitle = c("Night", "Morning", "Afternoon", "Evening")

#covid columns which are in the data frame
covidCountColumns = c(
  'borough_case_count', 
  'borough_case_count_7day_avg', 
  'borough_hospitalized_count', 
  'borough_hospitalized_count_7day_avg',
  'borough_death_count', 
  'borough_all_death_count_7day_avg')

#restriction columns which are in the data frame
restrictionColumns = c("high_schools", "indoor_religious" ,"stadium_capcity" , "restaurants")

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
#OPTIMZING MODELS
#
#makes an balnk list
modelList <- list(seq(length(timesInDay)))

#for each part of the day
for(timeIndex in seq(length(timesInDay))) {
  
  #update to console
  print(paste("part of day",timeIndex))

  #constructs name of the 2019 respective taxi count column
  splitTime <- strsplit(timesInDay[timeIndex], "2020")
  column2019 = paste0(splitTime[[1]][1], "2019", splitTime[[1]][2])
  
  #creates vectors of the other features
  otherFeatures = c("week")
  caterFeatures = union(restrictionColumns, c("day_week", "borough", "fhv"))
  continFeatures = union(column2019, covidCountColumns)
    

  
  #model is of the current part of the day to the rest of the data set
  formula = paste0(timesInDay[timeIndex]," ~ +(",
                   do.call(
                     #columns with no interaction
                     paste, c(as.list(otherFeatures), sep = " +")
                   ),") +(",do.call(
                     #categorical columns
                     paste, c(as.list(caterFeatures), sep = " +")
                   ),") +(",do.call(
                     #continuous columns
                     paste, c(as.list(continFeatures), sep = " +")
                   ),")"
  )
  
  #adds a fitted and optimised model (via AIC and step method) to the list
  modelList[[timeIndex]] <- step(lm( 
    as.formula(formula),
    
    data = dfTotal
  ),trace=0) #stops printing out aic steps
}


#loops and prints the summary for each model
for(timeIndex in seq(length(timesInDay))) {
  print(summary(modelList[[timeIndex]]))
}

#█ ▀█ █▄▄▀ █▄▄█ █▄▄�▄▄█ █▄▄�▄▄█
#SIG LEVEL TESTING FUNCTION
#
# takes a column attribute and finds its importance in the different 
ci = 0
FindColumnSig <- function(fittedList, columnName, others = c(), sigLevel = 0.05) {
  
  #sets sig count variables to 0
  sig = 0
  count = 0
  
  #for each of the bins
    for(timeIndex in seq(length(timesInDay))) {
      
      
      #creates a model that has
      #the column of interest
      modelWith <- lm(
        as.formula(
          paste0(timesInDay[timeIndex]," ~ ",
                 do.call(paste, c(as.list(
                   union( 
                     #combines old attibutes
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
                   setdiff( 
                     #finds old attibutes
                     attr(terms(fittedList[[timeIndex]]), "term.labels"),
                     c(columnName, others))), sep = " + ")  #removes new attibutes
                 )
          )
        ),
        data = dfTotal
      )
      
      
      #if there wasn't a change to the model between the two
      if ( anova(modelWith, modelWout)$Df[2] == 0) {
        
        #clearly wasn't significant
        count = count + 1
      }else{
        
        #otherwise checks the sig level
        #counts the model if the relevant term was significant
        sig = sig + if (anova(modelWith, modelWout)$`Pr(>F)`[[2]] < sigLevel) 1 else 0
        count = count + 1
      }
      
    }
  
    #returns the percentage of models which were significant
    return( sig/ count )
}

#█ ▀█ █▄▄▀ █▄▄█ █▄▄�▄▄█ █▄▄�▄▄█ 
# TESTING FUNCTIONS

#testing out the function with sig level of wekek
(out = FindColumnSig(modelList, "week"))


#checks sig level for all the covid count columns
for( i in covidCountColumns) {
  print(i)
  print(FindColumnSig(modelList, i,  sigLevel = 0.01))
}

#checks sig level of removing both sig columns
print(FindColumnSig(modelList, "borough_case_count_7day_avg", "borough_all_death_count_7day_avg"))


#checks sig level for all the restriction levels
for( i in restrictionColumns) {
  out = FindColumnSig(modelList, i,  sigLevel = 0.01)
  print(i)
  print(out)
}

#checks sig level of dropping both sig columns
print(FindColumnSig(modelList, "stadium_capcity", "indoor_religious"))
