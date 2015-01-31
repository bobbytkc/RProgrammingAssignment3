rankall <- function(outcome, num = "best") {
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        
        if( outcome != "heart attack" & outcome != "heart failure" &outcome != "pneumonia" ){
                stop("invalid outcome")
        }
        
        statedata <- split(data, data[,7])
        if(outcome == "heart attack" ) colNum <- 11
        if(outcome == "heart failure" ) colNum <- 17
        if(outcome == "pneumonia" ) colNum <- 23
        
        findRankData<-function(statedata, colNum, num){
                
                
                suppressWarnings(statedata[,colNum]<-as.numeric(statedata[,colNum]))
                orderList <- order(statedata[,colNum],statedata[,2])
                statedata <- statedata[orderList,]
                statedata <-statedata[!is.na(statedata[,colNum]),]
                
                if(num=="best") return(statedata[1,])
                if(num=="worst") return(statedata[length(statedata[,1]),])
                if(num>length(statedata[,1])) {
                        temp<-rep(NA, length(statedata[1,]))
                        temp[7] <- statedata[[1,7]]
                        temp<-matrix(temp,1,length(statedata[1,]))
                        return(temp)
                }
                statedata[num,]
        }
        
                
                
        rankData<-lapply(statedata,findRankData, colNum = colNum, num = num)
        
        #rankData<-rankData[!is.na(rankData)]
        
        stateList = c()
        hospitalList = c()
        
        for (i in 1:length(rankData)){
               
                hospitalList <- c(hospitalList,rankData[[i]][1,2])
                stateList <- c(stateList,rankData[[i]][1,7])
         }
       
       
        
        data.frame(hospital = hospitalList, state = stateList, row.names = stateList)
                
      
       
        
}