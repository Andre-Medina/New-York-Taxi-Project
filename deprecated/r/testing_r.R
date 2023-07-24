#█ █▀▄▀█ █▀█ █▀█ █▀█ ▀█▀ █ █▄ █ █▀▀ 
#█ █ ▀ █ █▀▀ █▄█ █▀▄  █  █ █ ▀█ █▄█ 
#

#main data directory
data_dir = "E:/2021/Applied Data Science/Project 1/Data/"

#name of csv file
csv_file = "taxi_data_tallyed_s2_neg.csv"

df_total <- read.csv(paste0(data_dir, csv_file))       #reads data
df_total <- df_total[setdiff(colnames(df_total),"X")]  #removes index column

#borough names in order
boroughs = c("Bronx","Brooklyn","Manhattan","Queens","Staten Island")

#times of the day in order
times = c("X2020_night", "X2020_morn", "X2020_arvo", "X2020_even")


df_total
pairs(df_total[])
pairs(df_total[
  (df_total["borough"] == 3 - 1) &
  (df_total["fhv"] == 0),][times])



set.seed(525354)                       # Set seed for reproducibility
N <- 1000                              # Sample size of 1000

x1 <- rnorm(N)                         # Create variable
x2 <- x1 + rnorm(N, 0, 3)              # Create correlated variable
x3 <- 2 * x1 - x2 + rnorm(N, 0, 2)     # Create another correlated variable

data <- data.frame(x1, x2, x3)         # Combine all variables to data.frame
pairs(data)     


#█▀▀ █ ▀█▀ ▀█▀ █ █▄ █ █▀▀    ▄▀█ █ █▀▀ 
#█▀  █  █   █  █ █ ▀█ █▄█    █▀█ █ █▄▄ 
#

#makes a blank array
dfm <- list(seq(length(boroughs)))

#for each borough
for(borough_i in seq(length(boroughs))){
  
  #makes another balnk list
  temp <- list(seq(length(times)))
  
  #update to console
  print(paste("borough",borough_i))
  
  #for each part of the day
  for(part_i in seq(length(times))){
  
    #update to console
    print(paste("part of day",part_i))
    
    #adds a fitted and optimised model (via AIC and step) to the array
    temp[[part_i]] <- step(lm( 
      as.formula(
        
        #model is of the current part of the day to the rest of the data set
        paste0(times[part_i]," ~. -",do.call(
          
            #less the other parts of the day
            paste, c(as.list(setdiff(times,c(times[part_i]))), sep = " -")
          )
        )
      ),
      
      #adds the data to be anything from the relativent borough
      data = df_total[df_total["borough"] == borough_i - 1,]
    ),trace=0) #stops printing out aic steps
  }
  
  #adds the temp list to the main list
  dfm[[borough_i]] <- temp
}


#loops and prints the summary
for(borough_i in seq(length(boroughs))){
  for(part_i in seq(length(times))){
    print(summary(dfm[[borough_i]][[part_i]]))
  }
}


#LLLOOOOKS LIKE ITS PRETTY EASY TO REMOVE INTERACITON
attr(terms(out[[1]]), "variables")
attr(terms(out[[1]]), "term.labels")
colnames(t(attr(terms(out[[1]]), "dataClasses")))



#█▀▀ █▀▀█ █▀▀▄ █▀▀  ▀  █▀▀▄ █▀▀ █▀▀▄ █▀▀ █▀▀    █▀▀▀ █▀▀█ █▀▀█ █▀▀█ █  █ 
#█   █  █ █  █ █▀▀ ▀█▀ █  █ █▀▀ █  █ █   █▀▀    █ ▀█ █▄▄▀ █▄▄█ █▄▄█ █▀▀█ 
#▀▀▀ ▀▀▀▀ ▀  ▀ ▀   ▀▀▀ ▀▀▀  ▀▀▀ ▀  ▀ ▀▀▀ ▀▀▀    ▀▀▀▀ ▀ ▀▀ ▀  ▀ █    ▀  ▀ 
# takes a column attribute and finds its importance in the different 
graph_ci_of <- function(fitted_frame, col_name, others = c(), sig_level = 0.05) {
  
  #creates an empty array for collating the confidence intervals
  conf_int_array <- list(c(),c())
  sig = 0
  count = 0
  
  #for each of the bins
  for(borough_i in seq(length(boroughs))){
    for(part_i in seq(length(times))){
      
      #adds the column of interest to the linear model
      model <- lm(
        as.formula(
          paste0(times[part_i]," ~ ",
            do.call(paste, c(as.list(
              union( #combines old attibutes
                attr(terms(fitted_frame[[borough_i]][[part_i]]), "term.labels"),
                c(col_name, others))), sep = " + ")  #adds new attibutes
            )
          )
        ),
        #only includes data from relavent borough
        data = df_total[df_total["borough"] == borough_i - 1,]
      )
      
      #finds the CI
      ci <- confint(model, col_name,level=(1-sig_level))
      
      #counts the model if the relavent term was significant
      sig = sig + if(summary(model)$coefficients[col_name,4] < sig_level) 1 else 0
      count = count + 1
      
      for(index in seq(2)){
        conf_int_array[[index]] <- c(conf_int_array[[index]],ci[index])
      }
    }
  }
  
  #finds the x max and x min for graphing by getting the max and min values
  high <- max(c(conf_int_array[[1]],conf_int_array[[2]]))
  if (high > 0) high*2 else (high/2)
  low <- min(c(conf_int_array[[1]],conf_int_array[[2]]))
  if (low < 0) low*2 else (low/2)
  
  #graphs the lowerbound 
  hist(conf_int_array[[1]], breaks=5, xlim=c(low, high), col=rgb(1,0,0,0.5), xlab="coefficient value", 
       ylab="frequency", main=paste0((1-sig_level)*100," % confidence interval for effect of ", col_name))
  
  # adds the second graph for the upper bound ontop
  hist(conf_int_array[[2]], breaks=5, xlim=c(low, high), col=rgb(0,0,1,0.5), add=T)
  
  #creates a legend in the top corner
  legend(
    "topright", 
    legend=c(
      paste0("Lower ",(sig_level / 2) * 100, "%"),     #writes low sig level
      paste0("Upper ",(1 - sig_level / 2) * 100, "%"), #writes high sig level
      paste0((sig/count)*100,"% signicant")),          #writes proportion that 
                                                       #were considered sig
    col=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5),rgb(1,1,1,0)), #adds colours 
    pt.cex=2, pch=15)
}

#testing out the function
graph_ci_of(dfm, "week", "day")
graph_ci_of(dfm, "week")
graph_ci_of(dfm, "day","week")
graph_ci_of(dfm, "day")

graph_ci_of("total_hospitalized_count_7day_avg")

