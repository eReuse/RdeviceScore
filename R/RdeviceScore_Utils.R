tryCatchError <- function(expr) {
  W <- NULL
  w.handler <- function(w){  
    W <<- w
    invokeRestart("muffleWarning")
  }
  list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                   warning = w.handler),
       warning = W)
}

# Rename columns of sourceData to fit with the program and ensure all required columns exists
adaptSchemaScore <- function(schema, sourceData){
  columnNames <- schema # Load Config file
  isError  <- inherits(columnNames$value, "simpleError")
  if(!isError){
    oldColNames <- columnNames$sourceSchema # Column 1
    newColNames <- columnNames$targetSchema
    columnsMap <- data.frame(oldColNames,newColNames)
    adaptedSchema <- setSchemaScore(sourceData,columnsMap)
    adaptedSchema <- adaptedSchema[,newColNames, with=FALSE] ## Filter so- 
  } else{
    adaptedSchema <- list(value = columnNames$value, warning = columnNames$warning) # colnames has the error
  }
  return(adaptedSchema)
}
writeFileScore <- function(fileOutput, dt){
  result <- tryCatchError(write.table(format(dt, digits=4, decimal.mark =","), file=fileOutput, col.names = TRUE,row.names = FALSE,append=FALSE, sep=";"))
}

setSchemaScore <- function(sourceData, columnsMap){
  #' A schema mapping is a specification that describes how data structured under one schema (the source schema) 
  #' is to be transformed into data structured under a different schema (the target schema)
  #' column names are renamed to match the score algoritm, if the source schema do not has a match column in the target
  #' schema then a column is created.  
  
  #' Example:
  #' oldColNames <- c('Model','Manufacturer')
  #' newColNames <- c('model','manufacturer')
  #' columnsMap <- data.frame(oldColNames,newColNames)
  
  #' This function renames columns in the sourceData to match with the target schema (algorith variables)
  #' TODO launch a warning if the source schema do not has a match column in the target schema
  #i<-8
  for(i in 1:nrow(columnsMap)) {
    oldColumnName <- as.character(columnsMap$oldColNames[i])
    newColumnName <- as.character(columnsMap$newColNames[i])
    if(length(names(sourceData)[names(sourceData) == oldColumnName])!=0){ # column to rename exists
      names(sourceData)[names(sourceData) == oldColumnName] <- newColumnName # rename the column
    } else { # column to rename not exists, if already not created, then create it with NA value
      if(length(names(sourceData)[names(sourceData) == newColumnName]) == 0){ # new column no exist, create it
        sourceData$temp <- NA # temporal column
        names(sourceData)[names(sourceData) == 'temp'] <- newColumnName # rename the column
      }
    }
  }
  return(sourceData)
}

writeScore <- function(dt1, file_all, file_filter){
  #'  Filter columns
  schema <- data.table(read.csv2(file=files$file.schema,sep = ";", fill = TRUE, dec = ",", na.strings = "NA", stringsAsFactors = FALSE, row.names = NULL))
  colsToFilter <- schema[schema$output==1, targetSchema]
  colsToFilterNewName <-schema[schema$output==1, outputName]
  
  # Code util.
  # newColNames <- read.csv2(file=files$file.schema,sep = ",", fill = TRUE, dec = ",", na.strings = "NA", stringsAsFactors = FALSE, row.names = NULL)$targetSchema
  # columnsMap <- data.frame(oldColNames,newColNames)
  # sourceData <- setSchema(sourceData,columnsMap)
  # sourceData <- sourceData[,newColNames, with=FALSE] ## Filter sourceData with only need it columns (because performance aspects)
  # 
  
  dt1_filter <- dt1[,colsToFilter, with=FALSE]
  setnames(dt1_filter,colsToFilterNewName)
  dt1_filter <- dt1_filter[order(-Price)]
  write.table(format(dt1, digits=4, decimal.mark ="."), file=file_all,col.names = TRUE,row.names = FALSE,append=FALSE, sep=";") 
  write.table(format(dt1_filter, digits=4, decimal.mark ="."), file=file_filter,col.names = TRUE,row.names = FALSE,append=FALSE, sep=";") 
  # End changes
  
  # columns <-c("Identifier","serial.number","state","type","processor","model","manufacturer","Processor.1.model","Processor.1.numberOfCores","Processor.1.score","Processor.1.speed", "Ram.size"
  #             ,"Ram.speed","Drive.size","Drive.readingSpeed","Drive.writingSpeed","Processor.score","Ram.score","Drive.score","Score","Price", "Range", "Cost.refurbisher", "Cost.circuit", "Cost.retailer")
  # dt1_filter <- dt1[,columns, with=FALSE]
  # columns_new <- c("Identifier","serial.number","state","type","processor","Model","Manufacturer","Processor.name","Processor.cores","Processor.score","Processor.speed", "Ram.size"
  #                  ,"Ram.speed","Drive.size","Drive.readingSpeed","Drive.writingSpeed","Processor.score","Ram.score","Drive.score","Score","Price", "Range", "Cost.refurbisher", "Cost.circuit", "Cost.retailer")
  # setnames(dt1_filter, columns_new)
  # dt1_filter <- dt1_filter[order(-Price)]
  # 
  # # Write data
  # write.table(format(dt1, digits=4, decimal.mark ="."), file=file_all,col.names = TRUE,row.names = FALSE,append=FALSE, sep=";") 
  # write.table(format(dt1_filter, digits=4, decimal.mark ="."), file=file_filter,col.names = TRUE,row.names = FALSE,append=FALSE, sep=";") 
}

writeStats <- function(){
  # TODO 
  checkDataQuality(data=dt1_filter, out.file.num = files[["file_filter.num"]], out.file.cat = files[["file_filter.cat"]])
} 

