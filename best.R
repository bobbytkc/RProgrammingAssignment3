best <- function(state, outcome){
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        if( !(state%in% data$State)) {
                stop("invalid state")
        }
        
        if( outcome != "heart attack" & outcome != "heart failure" &outcome != "pneumonia" ){
                stop("invalid outcome")
        }
        
        statebinary <- data$State == state
        statedata <- data[statebinary,]
        
        
        
        if(outcome == "heart attack" ){
                
                suppressWarnings(statedata[,11]<-as.numeric(statedata[,11]))
                heartAttackOrder <- order(statedata[,11],statedata[,2] )
                orderedData <- statedata[heartAttackOrder,]
                orderedData <- orderedData[!is.na(orderedData[,11]),]
                
                
        }
        
        if(outcome == "heart failure" ){
                
                suppressWarnings(statedata[,17]<-as.numeric(statedata[,17]))
                heartFailureOrder <- order(statedata[,17],statedata[,2])
                orderedData <- statedata[heartFailureOrder,]
                orderedData <- orderedData[!is.na(orderedData[,17]),]
                
        }
        
        if(outcome == "pneumonia" ){
                suppressWarnings(statedata[,23]<-as.numeric(statedata[,23]))
                pneumoniaOrder <- order(statedata[,23],statedata[,2])
                orderedData <- statedata[pneumoniaOrder,]
                orderedData <- orderedData[!is.na(orderedData[,23]),]
                
        }
        
        orderedData[1,2]
}