#' Version: 0.2.0
#' 
#' Using partially Processor.1.score see comment https://tree.taiga.io/project/ereuseorg/us/6
#' Values under xMin are set to 0
#' Changes
#' v.0.2.0
#' - Add range, cost.refurbisher, cost.circuit, cost.retailer
#' - Comments code
#' - functions setScore include upgrade parameter
#' - testing improvements
#' v.0.2.1
#' - Appearance added to score, this only applies to the mid and high range
#' - Functionallity added to score
#' - Data filtered by type computer
#' - Filtering to only computers
#' - Filter result, add selector param in shema: Update schema.csv with columns to select
#' - Rscript to run in batch mode
#' TODO
#' - Filter result, add selector param in shema
#' - Add costs update and margins
#' - Reviu initDeviceScoreFiles "files are copyed outside test folder"
#' Add Constants "processor.weight" ... to model.csv
#' - Include a new test modifing test 1 version 0
#' 
#' - add appearance, functionality to the SCORE
#' 
#' 
#' 
#' 
#' 
#' TODO:
#' -  include in dataSchemaMapping the version of the schema to avoid working with folders 
#'  - reference tables (grade, manufacturer, score)
#'  - process esthetics (wait until new json from xavier)
#'  - input/return json
#'  - A new function (upgrade) that sets default values for missing components. price, score, rang will change with 
#'    new values would it has if you added missing components
#'  - normalize score from 0 to 10
#'  - add optivcal drive
#'  - add network card

deviceScoreMain <- function(input){
  
  sourceDataAux <- data.table(input$sourceData)
  schemaAux     <- data.table(input$schema)
  configAux     <- input$config
  versionSchema <- input$versionSchema
  versionScore  <- input$versionScore
  versionEntity <- input$versionEntity
  bUpgrade      <- input$bUpgrade
  
  if(ncol(configAux) == 1){ # if only 1 is because csv file is not structured 
    status <- "the character separator of the configuration file must be ; separator and decimals as coma separator"
    result <- simpleError(status)
  } 
  else {
    config        <- data.frame(configAux[configAux$param2 == versionEntity | configAux$param3==versionScore,c("param4","value1","value2","value3")],row.names = "param4") # load parameters afecting model and define key as param2
    schema        <- data.frame(schemaAux[schemaAux$version == versionSchema]) # load p
    sourceData    <- tryCatchError(adaptSchemaScore(schema,sourceDataAux))$value # Etl Schema ensure required columns exists
    isErrorSchema <- inherits(sourceData, "simpleError") 
    if(!isErrorSchema){
      result <- deviceScore(sourceData, config, bUpgrade) # TODO: Control errors
    } else {
      result <- simpleError(sourceData)
    }
  }
  return(result)
}
deviceScore  <- function(sourceData, config, bUpgrade){
  
  ##### Auxiliar functions   ####
  # (X-Xmin) / (Xmax-Xmin)
  # test: setNorm(8192,256, 8192)
  setNorm <- function(x, xMin, xMax){
    out <- (x - xMin) / (xMax - xMin) 
    return(out)
  }
  setScoreLin <- function(x){
    out <- 7*x + 0.06
    return(out)
  }
  setScoreExp <- function(x){
    out <- exp(x)/(2-exp(x))
    return(out)
  }
  setScoreLog <- function(x){
    out <- log10(2*x) + 3.57
    return(out)
  }
  getCpuSpeed <- function(x){
    #' test: x <- "Intel(R) Core(TM)2 Duo CPU E4600 @ 2.40GHz" result: 2.4
    #' test: x <- "Intel(R) Core(TM)2 Duo CPU E4600 @ GHz" result: NA
    #' TODO: Not extract AMD info
    regexp <- "[[:digit:]]+\\.*[[:digit:]]+GHz"
    text <-  str_extract(x, regexp)
    regexp <- "[[:digit:]]+\\.*[[:digit:]]"
    out<-as.numeric(str_extract(text, regexp))
    return(out)
  }
  is.nan.data.frame <- function(x) {
    do.call(cbind, lapply(x, is.nan))
  }
  harmonic_mean_2 <- function(chars, scores){
    # Component with two chars
    #' test:
    #' char <- list(c1=0.7,c2=0.3)
    #' scores <- list(s1=1.086, s2=1.167)
    #' harmonic_mean_2(list(c1=0.7,c2=0.3),list(s1=1.086, s2=1.167))
    out <- (chars$c1 + chars$c2) / ((chars$c1/scores$s1)+(chars$c2/scores$s2))
    return(out)
  }
  harmonic_mean_3 <- function(chars, scores){
    out <- (chars$c1 + chars$c2 + chars$c3) / ((chars$c1/scores$s1)+(chars$c2/scores$s2)+(chars$c3/scores$s3))
    return(out)
  }
  ##### Config parameters ####
  #' Create variables based on config values
  for(i in 1:nrow(config)){ # create required variables
    assign(rownames(config)[i], as.double(config[rownames(config)[i],"value1"])) # create a variable 
    #cat(i,": ",as.double(config[rownames(config)[i],"value1"]),"\n")
  }
  ##### Add date column #####
  #sourceData$createdData <- as.Date(sourceData$created) ## Add a column with class type date
  ##### Filter by date #####
  #sourceData <- sourceData[year(createdData) == year(as.Date("2017-01-01"))] ## Only devices created in 2017
  ##### Filter by state ####
  #sourceDataTmp <- sourceData[state == ("devices:Snapshot")] ## Only devices created in 2017
  
  ##### Filter Computers ####
  sourceData <- sourceData[type == "Computer"] ## Only devices created in 2017
  ##### Remove non sense columns   #####
  #' columns "Processor-3-model", "Processor-3-score", "Processor-4-score"
  sourceData$Processor.2.numberOfCores <- NULL
  sourceData$Processor.2.score <- NULL
  sourceData$Processor.2.model <- NULL
  sourceData$Processor.3.model<- NULL
  sourceData$Processor.3.numberOfCores<- NULL
  sourceData$Processor.3.score <- NULL
  sourceData$Processor.4.model  <- NULL
  sourceData$Processor.4.numberOfCores <-NULL
  sourceData$Processor.4.score <- NULL
  
  ##### Add id #####
  sourceData$id <- seq(1, length.out=nrow(sourceData), by=1) # add id
  dt1 <- sourceData
  ##### Fill 0 in apearance with ND #####
  for(i in 1:nrow(dt1)){ # Value 0 is considered maximu score so we set to ND: Non Defined
    if(is.na(dt1$appearance[i])){
      dt1$appearance[i] = "ND"
    }
  }
  ##### Fill 0 in funtionnality with ND #####
  for(i in 1:nrow(dt1)){ # Value 0 is considered maximu score so we set to ND: Non Defined
    if(is.na(dt1$functionality[i])){
      dt1$functionality[i] = "ND"
    }
  }
  ##### Fill NA with 0.0 #####
  dt1[is.na(dt1)] <- as.numeric(0.0)
  dt1[is.nan(dt1)] <- as.numeric(0.0)
  
  ##### Fill in values #####
  #### Upgrade ####
  #' Only are upgraded devices without any component of the upgrade$type
  #' Upgraded value update always first component (HardDrive.1 or RamModule.1)
  if(bUpgrade){
    #' Drive Size
    #upgrade <- list(disc=TRUE, disc.amount=150000, ram = TRUE, ram.amount = 2048)
    if(upgrade$disc==TRUE){
      dt1$Drive.number <- dt1[,(HardDrive.1.size != 0) + (HardDrive.2.size != 0) + (HardDrive.2.size != 0)]
      dt1[dt1$Drive.number == 0, HardDrive.1.size := upgrade$disc.amount] # update size with new value
      dt1[dt1$Drive.number == 0, upgrade.disc := 1]
      dt1[dt1$Drive.number == 0, upgrade.disc.amount := upgrade$disc.amount]
    }
    #' Ram Size
    if(upgrade$ram==TRUE){
      dt1$Ram.number <- dt1[,(RamModule.1.size != 0) + (RamModule.2.size != 0) + (RamModule.3.size != 0) + (RamModule.4.size != 0) + (RamModule.5.size != 0)]
      dt1[dt1$Ram.number == 0, RamModule.1.size := upgrade$ram.amount] # update size with new value
      dt1[dt1$Ram.number == 0, upgrade.ram := 1]
      dt1[dt1$Ram.number == 0, upgrade.ram.amount := upgrade$ram.amount]
    }
  }
  #### Estimate characteristics: hard drive ####
  dt1[dt1$HardDrive.1.readingSpeed == 0, HardDrive.1.readingSpeed := HardDrive.1.size / drive.readingSpeed.factor]
  dt1[dt1$HardDrive.2.readingSpeed == 0, HardDrive.2.readingSpeed := HardDrive.2.size / drive.readingSpeed.factor]
  dt1[dt1$HardDrive.3.readingSpeed == 0, HardDrive.3.readingSpeed := HardDrive.3.size / drive.readingSpeed.factor]
  dt1[dt1$HardDrive.1.writingSpeed == 0, HardDrive.1.writingSpeed := HardDrive.1.size / drive.writingSpeed.factor]
  dt1[dt1$HardDrive.2.writingSpeed == 0, HardDrive.2.writingSpeed := HardDrive.2.size / drive.writingSpeed.factor]
  dt1[dt1$HardDrive.3.writingSpeed == 0, HardDrive.3.writingSpeed := HardDrive.3.size / drive.writingSpeed.factor]
  #### Estimate characteristics: Memory speed ####
  dt1[dt1$RamModule.1.speed == 0, RamModule.1.speed := RamModule.1.size / ram.speed.factor]
  dt1[dt1$RamModule.2.speed == 0, RamModule.2.speed := RamModule.2.size / ram.speed.factor]
  dt1[dt1$RamModule.3.speed == 0, RamModule.3.speed := RamModule.3.size / ram.speed.factor]
  dt1[dt1$RamModule.4.speed == 0, RamModule.4.speed := RamModule.4.size / ram.speed.factor]
  dt1[dt1$RamModule.5.speed == 0, RamModule.5.speed := RamModule.5.size / ram.speed.factor]
  #### Set characteristics: Processor speed from model description #### 
  #' Processor Speed should be created extracting speed value from model field
  dt1[, Processor.1.speed := getCpuSpeed(Processor.1.model)] # Extract speed from model
  dt1[is.nan(Processor.1.speed), Processor.1.speed := 0] # Remove infinits
  dt1[is.na(Processor.1.speed), Processor.1.speed := 0] # Remove NA
  #### Estimate characteristics: Processor speed, cores and score #### 
  dt1[Processor.1.speed == 0, Processor.1.speed := processor.default.speed] # Set 0 values to 1.6 (p5)
  dt1[Processor.1.numberOfCores == 0, Processor.1.numberOfCores := processor.default.cores] # Set 0 values to 1
  dt1[Processor.1.score == 0, Processor.1.score := processor.default.score] # Set 0 values to 4000
  
  ######################### Step Fusion components. #########################
  #' Two (or n) components of same type are fusioned to one. we pondered by size
  ## Step 1.1 Memory
  #' Ex: RAMMemory 1 with 2GB, speed 100, RAMMemory 2 with 4GB, speed 200 results in 6GB with speed 166 (TODO: The computer only use the mínimum 
  #' RAM speed, can not work with diferent speed RAM)
  #' formula: Ram.speed = (RamModule.1.size x RamModule.1.speed + RamModule.2.size x RamModule.2.speed)/ (RamModul1.1.size + RamModule.2.size)
  dt1$Ram.size   <- dt1[,RamModule.1.size + RamModule.2.size + RamModule.3.size + RamModule.4.size + RamModule.5.size]
  #' TODO: Take only the minimum speed
  #' TODO: Calculate score for each RAM module
  
  dt1$Ram.speed  <- dt1[, (RamModule.1.size * RamModule.1.speed + RamModule.2.size * RamModule.2.speed +
                             RamModule.3.size * RamModule.3.speed + RamModule.4.size * RamModule.4.speed +
                             RamModule.5.size * RamModule.5.speed) / dt1$Ram.size]
  
  dt1[is.nan(Ram.speed), Ram.speed := 0.0] # Remove infinites in Ram speed
  
  #### harddrive ####
  # Ex: hard drive 1 with 40 GB writing 100, writing 100, hard 2 with 80GB, writing 200, writing 200, results in 120 GB reading 166, writing 166
  # eg: (disc.1.readingSpeed x disc.1.size  + disc.2.readingSpeed x disc.2.size) / disc.size
  
  dt1$Drive.size   <- dt1[,HardDrive.1.size + HardDrive.2.size + HardDrive.3.size]
  
  dt1$Drive.readingSpeed <- dt1[,(HardDrive.1.readingSpeed * HardDrive.1.size + 
                                    HardDrive.2.readingSpeed * HardDrive.2.size + 
                                    HardDrive.3.readingSpeed * HardDrive.3.size) /
                                  Drive.size]
  dt1[is.nan(Drive.readingSpeed), Drive.readingSpeed := 0.0] # Remove infinites
  
  dt1$Drive.writingSpeed <- dt1[,(dt1$HardDrive.1.writingSpeed * HardDrive.1.size + 
                                    dt1$HardDrive.2.writingSpeed * HardDrive.2.size + 
                                    dt1$HardDrive.3.writingSpeed * HardDrive.3.size) /
                                  dt1$Drive.size]
  dt1[is.nan(Drive.writingSpeed), Drive.writingSpeed := 0.0] # Remove infinites
  
  #### Speed CPU ####
  
  # There are values with same processor but differend scores, we are not going to use processor.1.score as unique value.in the calculation, see example in coments
  # Example:
  # aux.cpu <- dt1[Processor.1.score!=0, c("id","created","model","Processor.1.model","Processor.1.score","Processor.1.speed","Processor.1.numberOfCores")]
  # aux.cpu.model <- aux.cpu[model=="HP d530 USDT(DZ034T)"]
  
  # Formula to calculate score. 
  # In case of i2, i3,.. result penalized. 
  # Example: Intel(R) Core(TM) i3 CPU 530 @ 2.93GHz, score = 23406.92 but results inan score of 17503
  
  dt1[,Processor.score.aux := (Processor.1.score + Processor.1.speed * 2000 * Processor.1.numberOfCores)/2]
  
  ######################### Step Normalization #########################
  #### RAM Normalization ####
  # Size
  dt1$Ram.size.norm <- dt1[,setNorm(Ram.size,ram.size.xMin,ram.size.xMax)]
  dt1[Ram.size.norm < 0, Ram.size.norm := 0] # set negative values to 0
  # Speed
  dt1$Ram.speed.norm <- dt1[,setNorm(Ram.speed,ram.speed.xMin,ram.speed.xMax)]
  dt1[Ram.speed.norm < 0, Ram.speed.norm := 0] # set negative values to 0
  #### DISC Normalization ####
  # Size
  dt1$Drive.size.norm <- dt1[,setNorm(Drive.size,drive.size.xMin,drive.size.xMax)]
  dt1[Drive.size.norm < 0, Drive.size.norm := 0] # set negative values to 0
  # Reading Speed
  dt1$Drive.readingSpeed.norm <- dt1[,setNorm(Drive.readingSpeed,drive.readingSpeed.xMin,drive.readingSpeed.xMax)]
  dt1[Drive.readingSpeed.norm < 0, Drive.readingSpeed.norm := 0] # set negative values to 0
  # Writing Speed
  dt1$Drive.writingSpeed.norm <- dt1[,setNorm(Drive.writingSpeed,drive.writingSpeed.xMin,drive.writingSpeed.xMax)]
  dt1[Drive.writingSpeed.norm < 0, Drive.writingSpeed.norm := 0] # set negative values to 0
  
  #### Processor Normalization ####
  # Score
  dt1$Processor.score.norm <- dt1[,setNorm(Processor.score.aux, processor.xMin, processor.xMax)]
  dt1[Processor.score.norm < 0, Processor.score.norm := 0] # set negative values to 0
  
  ######################### Step Score. #########################
  #' TODO. Create a table with all scores and avoid does this calculations
  #### Score RAM ####
  # Size
  setkey(dt1,Ram.size.norm)
  dt1[(Ram.size.norm >= cexp)&(Ram.size.norm < clin),Ram.size.score := setScoreExp(Ram.size.norm)]
  dt1[(Ram.size.norm >= clin)&(Ram.size.norm<clog),Ram.size.score := setScoreLin(Ram.size.norm)]
  dt1[(Ram.size.norm >= clog),Ram.size.score := setScoreLog(Ram.size.norm)]
  # Speed
  setkey(dt1,Ram.speed.norm)
  dt1[(Ram.speed.norm >= cexp)&(Ram.speed.norm < clin),Ram.speed.score := setScoreExp(Ram.speed.norm)]
  dt1[(Ram.speed.norm >= clin)&(Ram.speed.norm<clog),Ram.speed.score := setScoreLin(Ram.speed.norm)]
  dt1[(Ram.speed.norm >= clog),Ram.speed.score := setScoreLog(Ram.speed.norm)]
  #### Score Drive ####
  # Size
  setkey(dt1,Drive.size.norm)
  dt1[(Drive.size.norm >= cexp)&(Drive.size.norm < clin),Drive.size.score := setScoreExp(Drive.size.norm)]
  dt1[(Drive.size.norm >= clin)&(Drive.size.norm<clog),Drive.size.score := setScoreLin(Drive.size.norm)]
  dt1[(Drive.size.norm >= clog),Drive.size.score := setScoreLog(Drive.size.norm)]
  # Writing Speed
  setkey(dt1,Drive.writingSpeed.norm)
  dt1[(Drive.writingSpeed.norm >= cexp)&(Drive.writingSpeed.norm < clin),Drive.writingSpeed.score := setScoreExp(Drive.writingSpeed.norm)]
  dt1[(Drive.writingSpeed.norm >= clin)&(Drive.writingSpeed.norm<clog),Drive.writingSpeed.score := setScoreLin(Drive.writingSpeed.norm)]
  dt1[(Drive.writingSpeed.norm >= clog), Drive.writingSpeed.score := setScoreLog(Drive.writingSpeed.norm)]
  # Reading Speed
  setkey(dt1,Drive.readingSpeed.norm)
  dt1[(Drive.readingSpeed.norm >= cexp)&(Drive.readingSpeed.norm < clin),Drive.readingSpeed.score := setScoreExp(Drive.readingSpeed.norm)]
  dt1[(Drive.readingSpeed.norm >= clin)&(Drive.readingSpeed.norm<clog),Drive.readingSpeed.score := setScoreLin(Drive.readingSpeed.norm)]
  dt1[(Drive.readingSpeed.norm >= clog), Drive.readingSpeed.score := setScoreLog(Drive.readingSpeed.norm)]
  # Step 3.3 Score Processor
  dt1[(Processor.score.norm >= cexp), Processor.score := setScoreExp(Processor.score.norm)]
  dt1[(Processor.score.norm >= clin)&(Processor.score.norm<clog), Processor.score := setScoreLin(Processor.score.norm)]
  dt1[(Processor.score.norm >= clog), Processor.score := setScoreLog(Processor.score.norm)]
  
  #### Score Appearance #####
  #'   Update appearance.score with the value of variable appearance.?
  #'   help: this function execute the variable appearance.b:  eval(parse(text=paste("appearance","B",sep=".")))
  dt1$appearance.score <- 0 #' Create Column appearance.score
  for(i in 1:nrow(dt1)){ # 
    if(dt1$appearance[i]!=""){
      eAppearance <- paste("appearance",dt1$appearance[i], sep=".")
      dt1$appearance.score[i] =  eval(parse(text=eAppearance))
    }
  }
  #### Score Functionallity #####
  
  dt1$functionality.score <- 0 #' Create Column appearance.score
  for(i in 1:nrow(dt1)){ # 
    if(dt1$functionality[i]!=""){
      eFunctionality <- paste("functionality",dt1$functionality[i], sep=".")
      dt1$functionality.score[i] =  eval(parse(text=eFunctionality))
    }
  }
  
  
  
  ######################### Step Fusion Charactersitics. #########################
  #' Result in a unic score per component. Harmonic Mean (weight)
  # Example: Ram.score = (Ram.size.score*x + Ram.speed.score*y)/z
  
  # Formula
  # component score = (char.1.weight + char.2.weigth) / ((char.1.weigth/char.1.score) + (char.2.weight/char.2.score))
  
  #### Fusion RAM ####
  # Size, Speed
  dt1$Ram.score <- dt1[,harmonic_mean_2(list(c1=ram.size.weight,c2=ram.speed.weight),
                                        list(s1=Ram.size.score, s2=Ram.speed.score))]
  
  #### Fusion Disc ####
  # Size, readingSpeed, writingSpeed 
  dt1$Drive.score <- dt1[,harmonic_mean_3(list(c1=drive.size.weight,c2=drive.readingSpeed.weight, c3 = drive.writingSpeed.weight),
                                          list(s1=Drive.size.score, s2=Drive.readingSpeed.score, s3 = Drive.writingSpeed.score))]
  
  #### Fusion Processor ####
  # Processor is not fusioned into components, score is itself an harmonic mean
  ######################### Step Fusion Components. #########################
  #' Result in a unic score
  #' Ex: processor 
  
  if(!exists("processor.weight")){  # Compatibility with old model versions
    processor.weight  <- 0.5
    drive.weight      <- 0.2
    ram.weight        <- 0.3 
  }
  dt1$Score.components <- dt1[,harmonic_mean_3(list(c1=processor.weight,c2=drive.weight, c3 = ram.weight),
                                               list(s1=Processor.score, s2=Drive.score, s3 = Ram.score))]
  
  ##### Step Sum Appearance in score #####
  # The Appearance score only applies to the mid and high range
  setkey(dt1,Score.components)
  dt1$Range.components = "VeryLow"
  dt1[(Score.components > range.components.medium), Range.components := "High"]
  dt1[(Score.components <= range.components.medium) & (Score.components > range.components.low), Range.components := "Medium"]
  dt1[(Score.components <= range.components.low) & (Range.components > range.components.inf), Range.components := "Low"]
  dt1[(Score.components <= range.components.inf), Range.components:= "VeryLow"]
  
  ##### Step Sum Functionallity and appearance in score #####
  # We add functionallity to any range
  dt1$Score.final = 0.0 
  dt1[,Score.final := Score.components + functionality.score + appearance.score]
  # Remove negatives scores
  dt1$Score = dt1$Score.final
  dt1[Score<0, Score := 0.0]
  
  ##### Update Range #####
  if(!exists("range.medium")){ # Compatibility with old model versions, if one exists others to
    range.medium <- 4
    range.low <- 3
    range.inf <- 2
  }
  
  dt1$Range = dt1$Range.components
  
  setkey(dt1,Score)
  dt1[(Score > range.medium), Range := "High"]
  dt1[(Score <= range.medium) & (Score > range.low), Range := "Medium"]
  dt1[(Score <= range.low) & (Score > range.inf), Range := "Low"]
  dt1[(Score <= range.inf), Range := "VeryLow"]
  
  if(nrow(dt1[is.na(Range)])!=0) print("error step 7 assign range")
  
  ######################### Step Pricing #########################
  #' 1 point 20 €
  
  # if(!exists("priceperpoint")){ # Compatibility with old model versions
  #   priceperpoint <- 20
  # }
  # 
  # dt1$Price <- dt1[,Score* priceperpoint]
  # 
  #' Step 7. Range
  #' Add range: high [66€,...], medium [51€,65€], low [40,50], inf [0,40]
  
  # if(!exists("range.medium")){ # Compatibility with old model versions, if one exists others to
  #   range.medium <- 65
  #   range.low <- 50
  #   range.inf <- 40
  # }
  
  # dt1$Range = dt1$Range.before.appearance
  # 
  # setkey(dt1,Price)
  # dt1[(round(Price) > range.medium), Range := "High"]
  # dt1[(round(Price) <= range.medium) & (round(Price) > range.low), Range := "Medium"]
  # dt1[(round(Price) <= range.low) & (round(Price) > range.inf), Range := "Low"]
  # dt1[(round(Price) <= range.inf), Range := "VeryLow"]
  # 
  # if(nrow(dt1[is.na(Range)])!=0) print("error step 7 assign range")
  # 
  
  #' Step 8.  Improvements
  #' The new score, range, price if some of missing components are added. 
  #' Missing components can be (Ram.size, Drive.size)
  #' we add a component(s) with score around p25
  
  # Score.upgrade.missing
  
  
  #' Assign costs
  #' Refurbisher: (high) 27.60, (medium) 22.60€, (low) 19.60€
  #' Circuit: (high)  15€, (medium) 14€, (low) 5.5€
  #' Retailer: (high) Price - cost.refurbisher - cost.circuit
  
  #' dt1[Range == "High", Cost.refurbisher := 27.60]
  #' dt1[Range == "Medium", Cost.refurbisher := 22.60]
  #' dt1[Range == "Low", Cost.refurbisher := 19.60]
  #' 
  #' dt1[Range == "High", Cost.circuit := 15]
  #' dt1[Range == "Medium", Cost.circuit := 14]
  #' dt1[Range == "Low", Cost.circuit := 5.5]
  #' 
  #' dt1[Range != "VeryLow", Cost.retailer := round(Price - Cost.circuit - Cost.refurbisher)]
  #' 
  #' #' Statistics for parnters
  #' #' 
  #' 
  #' dt1.Costs.mean <- dt1[,.(avg.refurbisher=mean(Cost.refurbisher), avg.circuit=mean(Cost.circuit), avg.retailer = mean(Cost.retailer)),by=.(Range)]
  #' 
  #' Return values
  result <- list("value" = dt1, "status" = 0, "description" = "All ok")
  return(result) 
  ################ Set Score If Upgrade Components  ################
  # dt.upgrade.disc <- setScore(files[["file.data"]],upgrade)$table
  # upgrade <- list(disc=FALSE, disc.amount=NULL, ram = TRUE, ram.amount = 2048)
  # dt.upgrade.ram <- setScore(files[["file.data"]],upgrade)$table
  # upgrade <- list(disc=TRUE, disc.amount=150000, ram = TRUE, ram.amount = 2048)
  # dt.upgrade.ramAndDisc <- setScore(files[["file.data"]],upgrade)$table
  ################# Set Other variables  ################
  # dt.normal[,sum(Price)]
  # dt.upgrade.disc[,sum(Price)]
  # dt.upgrade.ram[,sum(Price)]
  # dt.upgrade.ramAndDisc[,sum(Price)]
  # dt.normal[,sum(Range=="inf")]
  # dt.upgrade.disc[,sum(Range=="inf")]
  # dt.upgrade.ram[,sum(Range=="inf")]
  # dt.upgrade.ramAndDisc[,sum(Range=="inf")]
  # 
  # # DISC
  # setkey(dt.normal,id)
  # dt.normal.aux <- dt.normal[,c("id","Price","Range")]
  # dt.upgrade.disc.aux <- dt.upgrade.disc[,c("id","upgrade.disc","Price","Range")]
  # colnames(dt.upgrade.disc.aux) <- c("id","upgrade.disc","Price.improved.disc","Range.improved.disc")
  # setkey(dt.upgrade.disc.aux,id)
  # dt.aux.disc <- dt.normal.aux[dt.upgrade.disc.aux, nomatch=0] # join
  # dt.aux.disc[,amount.improved.disc:= Price.improved.disc - Price]
  # dt.aux.disc[,inc.pv.improved.disc := amount.improved.disc / Price]
  # dt.result.disc <- dt.aux.disc[upgrade.disc == 1 & Range.improved.disc!="inf",]
  # 
  # # RAM
  # setkey(dt.normal,id)
  # dt.upgrade.ram.aux <- dt.upgrade.ram[,c("id","upgrade.ram","Price","Range")]
  # colnames(dt.upgrade.ram.aux) <- c("id","upgrade.ram","Price.improved.ram","Range.improved.ram")
  # setkey(dt.upgrade.ram.aux,id)
  # dt.aux.ram <- dt.normal.aux[dt.upgrade.ram.aux, nomatch=0] # join
  # dt.aux.ram[upgrade.ram == 1,amount.improved.ram:= Price.improved.ram - Price]
  # dt.result.ram <- dt.aux.ram[upgrade.ram == 1 & Range.improved.ram != "inf",]
  # setkey(dt.result.ram,id)
  # setkey(dt.result.disc,id)
  # 
  # dt.result <- dt.result.disc[dt.normal]
  # setkey(dt.result,Price)
  # dt.result <- dt.result.ram[dt.result]
  # 
  # writeScore(dt.aux,files[["file_all"]], files[["file_filter"]])
}
deviceScoreUnitest <- function(files, config){
  #' TODO: ad idTest as a parameter and update config
  #' copy data to process and to compare (from APP) to folder where simulation is done
  #' run the score
  #' compare results with test file
  result <- deviceScoreMain(files, config) # TODO return result!!!!!!
  file.compare <- paste0(files$paths$path.result.folder,"/","data_scoring_all.csv")
  result.data <- data.table(data.matrix(fread(format(files$file_all, digits=4, decimal.mark =".")))) # result of device score
  a <- result.data[,c("id","Score")]
  a$Score <- as.numeric(format(a$Score,digits = 4, na.encode = TRUE))
  dataToCompare <- data.table(data.matrix(fread(format(file.compare, digits=4, decimal.mark ="."))))
  b <- dataToCompare[,c("id","Score")]
  out <- anti_join(a,b) # Show diferences: values in a not present in b
  result <- list(
    result.value   = if(nrow(out)==0) 1, # ok
    result.description = if(nrow(out)==0) "All test ok",
    result.out = out
  )
  return(result)
}
