#█ █▀▄▀█ █▀█ █▀█ █▀█ ▀█▀ █ █▄ █ █▀▀ 
#█ █ ▀ █ █▀▀ █▄█ █▀▄  █  █ █ ▀█ █▄█ 
#
install.packages("ggplot2")
library("ggplot2")

#main data directory
data_dir = "E:/2021/Applied Data Science/Project 1/Data/"

#name of csv file
csv_file = "taxi_data_tallyed_s2_neg.csv"

df_total <- read.csv(paste0(data_dir, csv_file))       #reads data
df_total <- df_total[setdiff(colnames(df_total),"X")]  #removes index column
df_total <- df_total[setdiff(colnames(df_total),"day_year")]  #removes index column

#borough names in order
boroughs = c("Bronx","Brooklyn","Manhattan","Queens","Staten Island")

#times of the day in order
times = c("X2020_night", "X2020_morn", "X2020_arvo", "X2020_even")

#covid cols which are in this df
covid_of_interests = c(
  'borough_case_count', 
  'borough_case_count_7day_avg', 
  'borough_hospitalized_count', 
  'borough_hospitalized_count_7day_avg',
  'borough_death_count', 
  'borough_all_death_count_7day_avg')


df_total


df_total$borough <- factor(df_total$borough)
df_total$fhv <- factor(df_total$fhv)
df_total$phase_1 <- factor(df_total$phase_1)
df_total$restaurants <- factor(df_total$restaurants)
df_total$high_schools <- factor(df_total$high_schools)
df_total$indoor_religious <- factor(df_total$indoor_religious )




#█▀▀ █ ▀█▀ ▀█▀ █ █▄ █ █▀▀    ▄▀█ █ █▀▀ 
#█▀  █  █   █  █ █ ▀█ █▄█    █▀█ █ █▄▄ 
#

  
#makes another balnk list
model_list <- list(seq(length(times)))

#for each part of the day
for(part_i in seq(length(times))){
  
  #update to console
  print(paste("part of day",part_i))
  
  #makes list of 2020 cols to exclude
  cols_2020 = setdiff(times,c(times[part_i]))
  
  #makes a list of the 2019 columns to exclude
  cols_2019 = c()
  for(val in cols_2020){
    split_val <- strsplit(val, "2020")
    cols_2019 = c(cols_2019, paste0(split_val[[1]][1],"2019",split_val[[1]][2]))
  }
  
  #model is of the current part of the day to the rest of the data set
  formula = paste0(times[part_i]," ~ . -",do.call(
      
      #less the other parts of the day
      paste, c(as.list(union(cols_2019, cols_2020)), sep = " -")
    )
  )
  
  print(formula)
  
  #adds a fitted and optimised model (via AIC and step) to the array
  model_list[[part_i]] <- step(lm( 
    as.formula(formula),
    
    data = df_total
  ),trace=0) #stops printing out aic steps
}


#loops and prints the summary
for(part_i in seq(length(times))){
  print(summary(model_list[[part_i]]))
}

  
#█▀▀ █▀▀█ █▀▀▄ █▀▀  ▀  █▀▀▄ █▀▀ █▀▀▄ █▀▀ █▀▀    █▀▀▀ █▀▀█ █▀▀█ █▀▀█ █  █ 
#█   █  █ █  █ █▀▀ ▀█▀ █  █ █▀▀ █  █ █   █▀▀    █ ▀█ █▄▄▀ █▄▄█ █▄▄█ █▀▀█ 
#▀▀▀ ▀▀▀▀ ▀  ▀ ▀   ▀▀▀ ▀▀▀  ▀▀▀ ▀  ▀ ▀▀▀ ▀▀▀    ▀▀▀▀ ▀ ▀▀ ▀  ▀ █    ▀  ▀ 
# takes a column attribute and finds its importance in the different 
ci = 0
graph_ci_of <- function(fitted_frame, col_name, others = c(), sig_level = 0.05) {
  
  
  #creates an empty array for collating the confidence intervals
  ci_lowers = c()
  ci_uppers = c()
  mids = c()
  
  sig = 0
  count = 0
  
  #for each of the bins
    for(part_i in seq(length(times))){
      
      #adds the column of interest to the linear model
      model <- lm(
        as.formula(
          paste0(times[part_i]," ~ ",
                 do.call(paste, c(as.list(
                   union( #combines old attibutes
                     attr(terms(fitted_frame[[part_i]]), "term.labels"),
                     c(col_name, others))), sep = " + ")  #adds new attibutes
                 )
          )
        ),
        #only includes data from relavent borough
        data = df_total
      )
      
      #finds the CI
      ci <- confint(model, col_name,level=(1-sig_level))
      
      #counts the model if the relavent term was significant
      sig = sig + if(summary(model)$coefficients[col_name,4] < sig_level) 1 else 0
      count = count + 1
      

      
      
      ci_lowers = c(ci_lowers, (ci[1]))
      ci_uppers = c(ci_uppers, (ci[2]))
      mids = c(mids, (model$coefficients[col_name])[[1]])
      
    }
  
  #finds the x max and x min for graphing by getting the max and min values
  high <- max(ci_uppers)
  if (high > 0) high*2 else (high/2)
  low <- min(ci_lowers)
  if (low < 0) low*2 else (low/2)
  
  
  data <- data.frame(x = seq(length(times)),
                           y = mids,
                           lower = ci_lowers,
                           upper = ci_uppers)  
  

  return( list(data, sig/count) )
  ggplot(data, aes(x, y)) +        # ggplot2 plot with confidence intervals
    geom_point() +
    geom_errorbar(aes(ymin = lower, ymax = upper))
  
  #creates a legend in the top corner
  #legend(
  #  "topright", 
  #  legend=c(
  #    paste0((sig/count)*100,"% signicant")),          #writes proportion that 
  #  #were considered sig
  #  col=c(rgb(1,1,1,0)), #adds colours 
  #  pt.cex=2, pch=15)
}

#testing out the function
out = graph_ci_of(model_list, "week", "day_week")

data = out[[1]]
sig_percent = out[[2]]

sig_percent



for( i in covid_of_interests){
  out = graph_ci_of(model_list, i,  sig_level = 0.1)
  print(i)
  print(out[[2]])
}
  




ggplot(data, aes(x, y)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper))



col = c("high_schools", "indoor_religious" ,"stadium_capcity" , "restaurants0.5")

cat = c("high_schools1", "indoor_religious0.5" ,"stadium_capcity" , "restaurants0.5")

for( i in cat){
  out = graph_ci_of(model_list, i,  sig_level = 0.1)
  print(i)
  print(out[[2]])
}


graph_ci_of(model_list, "week")
graph_ci_of(model_list, "day_week","week")
graph_ci_of(model_list, "day_week")

graph_ci_of("total_hospitalized_count_7day_avg")






setdiff(attr(terms(model_list[[part_i]]), "term.labels"), c("day_week"))  





list = setdiff(times,c(times[part_i]))

cols_2019 = c()

  
cols_2019
union(setdiff(times,c(times[part_i])), cols_2019)


cols_2019
cols_2020      


test <- lm(X2020_even ~ . - X2019_night - X2019_morn - X2019_arvo - X2020_night - 
             X2020_morn - X2020_arvo, data=df_total)




set.seed(923874)                 # Create example data
data <- round(data.frame(x = 1:10,
                         y = runif(10, 10, 20),
                         lower = runif(10, 0, 10),
                         upper = runif(10, 20, 30)
                         ), 2)
data  
ggplot(data, aes(x, y)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper))
