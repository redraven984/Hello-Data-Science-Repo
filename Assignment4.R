# This contain three functions that were created as part of an assignment for week4
# of the Coursera course on R Programming. 

# The first function loads a set of hospital data that contains statistics on treatment
# outcomes, the function then finds the hospital with the best outcome subject to a state 
# input and a outcome input.

best <- function(state, outcome) {

  ## Read outcome data
  x.df <- read.csv("outcome-of-care-measures.csv", check.names=FALSE, colClasses = "character")
  
  ## Check if the input state exists and check if the outcome exists
  stateFlag<- state %in% x.df$"State"
  outcomeFlag <- grep(outcome, colnames(x.df))
  
  # Now if the state and output are correct, find the best outcome.
  
  if(stateFlag=="FALSE"){
      print("Incorrect State")
  }else if (sum(outcomeFlag)==0){
    print("Incorrect Outcome")
  } else {
    
    # From the loaded data frame, extract the subset of data corresponding to the
    # input state. 
    subset<- x.df[x.df$State==state,]
    
    # now combine the inputed outcome with the generic prefix to find the desired
    # data frame column 
    heading<-paste("Hospital 30-Day Death (Mortality) Rates from ",outcome,sep="")
    
    # Using the heading we just created find the index of the best outcome.            
    minRateLoc<-which.min(subset[,heading])
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    subset[minRateLoc,"Hospital Name"]
  }
 
  
}

# The second function loads a set of hospital data that contains statistics on treatment
# outcomes, the function then finds the hospital with the n^th rank in terms of outcome subject to the input state.
hospitalRank <- function(state, outcome,num="best") {
  
  ## Read outcome data
  x.df <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE, check.names=FALSE, colClasses = "character")
  
  # check to see if the input rank is best, if so, convert this to the number index 1.
  if(num=="best"){
    num<-1
  } 
  
  
  ## Check if the input state exists and check if the outcome exists
  stateFlag<- state %in% x.df$"State"
  outcomeFlag <- grep(outcome, colnames(x.df))
  
  # Now if the state and output are correct, find the ranked outcome.
  if(stateFlag=="FALSE"){
    print("Incorrect State")
  }else if (sum(outcomeFlag)==0){
    print("Incorrect Outcome")
  } else {
    
    # From the loaded data frame, extract the subset of data corresponding to the
    # input state. 
    subset<- x.df[x.df$State==state,]
    
    # now combine the inputed outcome with the generic prefix to find the desired
    # data frame column 
    heading<-paste("Hospital 30-Day Death (Mortality) Rates from ",outcome,sep="")
    
    # Using the heading we just created convert this column to numeric format, this fixes the problem
    # of NAs causing the problem to be read as character
    subset[,heading] <-as.numeric(subset[,heading])
    
    # Now use the order function to sort the column from best outcome to worst outcome. Ties are
    # broken by sorting althabetically by the Hospital Name column. NAs are sorted last.
    subset<-subset[order(subset[,heading],subset[,"Hospital Name"],na.last=TRUE),]
    
    # If the input rank was worst, then we need to find the last hospital with a numerical outcome rate. 
    if (num=="worst"){
      num<-nrow(subset) - sum(is.na(subset[,heading]))
    }
    
    ## Return hospital name in that state with n^th rank of 30-day death
    ## rate
    subset[num,"Hospital Name"]
  }
  
  
}

# The third function loads a set of hospital data that contains statistics on treatment
# outcomes, the function then finds the hospital with the n^th rank in terms of outcome for each state in the data set.
rankAll <- function(outcome,num="best") {
  
  ## Read outcome data
  x.df <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE, check.names=FALSE, colClasses = "character")

  ## Check if the outcome exists
  outcomeFlag <- grep(outcome, colnames(x.df))
  
  # Now if the output is correct, find the ranked outcome for each state.
  if (sum(outcomeFlag)==0){
    print("Incorrect Outcome")
  } else {
    
    # now combine the inputed outcome with the generic prefix to find the desired
    # data frame column 
    heading<-paste("Hospital 30-Day Death (Mortality) Rates from ",outcome,sep="")

    # Using the heading we just created convert this column to numeric format, this fixes the problem
    # of NAs causing the problem to be read as character
    x.df[,heading] <-as.numeric(x.df[,heading])
    
    # Now sort the mortality rate in an ascending fashion and break ties by alphabetizing by state.
    x.df<-x.df[order(x.df[,heading],x.df[,"State"]),]
    
    # We need a vector of states over which to iterate our ranking, so extract the state names from 
    # the data frame and then use unique to throw away the repeats.
    statesVect <-unique(x.df[,"State"])
    
    # Create an empty results data frame to store the results below
    results<-data.frame("State"=NA, "Hospital Name"=NA)
    
    # Now loop through the state vector selecting the nth result
    for(item in statesVect){
      
      # For the given state in "item" take a subset of the data frame for this state.
      stateSlice <- x.df[x.df$State==item,]

      # check to see if the input rank is best, if so, convert this to the number index 1.
      if(num=="best"){
        num<-1

        # If the input rank was worst, determine for this state, what is the index of the last hospital with
        # a numeric mortality index (after this index all hospitals have missing data, NA)
      } else if (num=="worst"){
        
        # Find the row number of the worst ranked hospital - find length of this data frame slice,
        # and subtract the number of missing data points which we have sorted to the end.
        num<-nrow(stateSlice) - sum(is.na(stateSlice[,heading]))
        print(num)
        
      } 
      
      # append the results to the data frame as a new row.
      results<-rbind(results,c(item,stateSlice[num,"Hospital Name"]))
      
    }
    ## Remove the first data frame row - creating an empty data frame introduces one empty row which
    # confuses outputs like list top 10 hospitals nationally by outcome.
    results <- results[-1,]
    
    ## Order the results data frame by state alphabetically
    results<- results[order(results[,"State"]),]
    
    ## Return the results data frame
    results
  }
  
  
}
