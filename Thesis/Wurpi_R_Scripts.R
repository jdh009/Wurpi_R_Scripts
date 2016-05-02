####################################### Metadata #######################################
###
### Developed by: Jeroen Roelofs
### Thesis, Wageningen University
###
###################################### Main script ######################################
###
### --------------------------------- Aquire and load libraries ------------------------
###
### Install packages if required.
# if (!require(xlsReadWrite)) install.packages('xlsReadWrite')
if (!require(gdata)) install.packages('gdata')
if (!require(timeDate)) install.packages('timeDate')
if (!require(timeSeries)) install.packages('timeSeries')

### Access package libraries
# library (xlsReadWrite)
library(gdata)
library(timeSeries)
library(timeDate)

### Set working directory
setwd("E:/WUR/Thesis/R/Wurpi_R_Scripts/Thesis")

###------------------------------------- Set variables -----------------------------------
### Set variables by user
ISset <- list(100, 400, 700) # IS settings of the camera
CSset <- list(-50, 0, 50)    # CS settings of the camera
EVset <- list(-2, 0, 2)      # EV settings of the camera


###------------------------------------- Converts variables -----------------------------------
### Converting variables into usefull stuff 

m <- list(ISset, CSset, EVset) # makes matrix of settings


###------------------------------------- loading csv ----------------------------------
heisenberg <- read.csv(file="LAIPiFinal.csv",head=TRUE,sep=",") # Read CSV file into memory

###------------------------------------- COnverts commas into dots  ------------------------------
heisenberg <- cbind(heisenberg, Miller_new = as.numeric(gsub(",", ".", gsub("\\.", "", heisenberg$Miller)))) ### COnverts commas into dots
heisenberg <- cbind(heisenberg, LAI2000_new = as.numeric(gsub(",", ".", gsub("\\.", "", heisenberg$LAI2000)))) ### COnverts commas into dots
heisenberg <- cbind(heisenberg, Lang_new = as.numeric(gsub(",", ".", gsub("\\.", "", heisenberg$Lang)))) ### COnverts commas into dots
heisenberg <- cbind(heisenberg, N...C_new = as.numeric(gsub(",", ".", gsub("\\.", "", heisenberg$N...C.)))) ### COnverts commas into dots
heisenberg <- cbind(heisenberg, T....al_new = as.numeric(gsub(",", ".", gsub("\\.", "", heisenberg$T....al)))) ### COnverts commas into dots




###------------------------------------- load benchmark Scripts ----------------------------------
# benchmark <- read.csv(file="LAIPiBenchmark.csv",head=TRUE,sep=",") # Read CSV file into memory
# for(linesincorrection in 1:length(correction)){ #Number that cycles throug different settings 1-4
#   subset_temp <- subset(benchmark, corrections == levels(benchmark$corrections)[1])
#   filename <- paste(as.character("benchmarkset"), as.character(as.character("1")), sep = "")
#   path <- "./Results/"
#   savelocation <- as.character(paste(as.character(path), as.character(filename), sep = ""))
#   write.csv(subset_temp, file = savelocation , quote = TRUE, eol = "\n", na = "NA", row.names = TRUE, fileEncoding = "")
#   
#   
#   } 

### 
# testbatch <- head(heisenberg, n=722)
# testbatch <- heisenberg
# df <- cbind(testbatch, var2 = as.numeric(gsub(",", ".", gsub("\\.", "", testbatch$Miller)))) ### COnverts commas into dots


### Creates diffent csv's per setting.
for(i in 1:length(m[[1]])){
  IS <- m[[1]][[i]]
  for(y in 1:length(m[[2]])){
    CV <- m[[2]][[y]]
    for(z in 1:length(m[[3]])){
      EV <- m[[3]][[z]]
      temp1 <- paste("IS", as.character(IS), sep = "")
      temp2 <- paste(as.character(temp1),"CV", sep = "")
      temp3 <- paste(as.character(temp2), as.character(CV), sep = "")
      temp4 <- paste(as.character(temp3), as.character("EV"), sep = "")
      settingstring <- paste(temp4, as.character(EV), sep = "")
      c <- NULL
      for(lines1 in 1:nrow(heisenberg)){
        if(grepl(settingstring, heisenberg$picture.files[lines1])){
          b <- heisenberg[lines1,]
          c <- rbind(c,b)
        }
      write.csv(c, file = as.character(settingstring), quote = TRUE,
                    eol = "\n", na = "NA", row.names = TRUE,
                    fileEncoding = "")
        
      }}}}



# settingstring <- "IS100CV0EV-2"
# dd<-data.frame(
#   saldt=seq(as.Date("1999-01-01"), as.Date("2014-01-10"), by="6 mon"),
#   salpr = heisenberg$th.hold)

## Create diffent csv's per setting with different corrections.
for(i in 1:length(m[[1]])){
  IS <- m[[1]][[i]]
  for(y in 1:length(m[[2]])){
    CV <- m[[2]][[y]]
    for(z in 1:length(m[[3]])){
      EV <- m[[3]][[z]]
      temp1 <- paste("IS", as.character(IS), sep = "")
      temp2 <- paste(as.character(temp1),"CV", sep = "")
      temp3 <- paste(as.character(temp2), as.character(CV), sep = "")
      temp4 <- paste(as.character(temp3), as.character("EV"), sep = "")
      settingstring <- paste(temp4, as.character(EV), sep = "")
      csv_without_corrections <- read.csv(file=as.character(settingstring),head=TRUE,sep=",") # Read CSV file into memory
      correction <- levels(csv_without_corrections$corrections)
      
      for(linesincorrection in 1:length(correction)){ #Number that cycles throug different settings 1-4
        subset_temp <- subset(csv_without_corrections, corrections == levels(csv_without_corrections$corrections)[linesincorrection])
        filename <- paste(as.character(settingstring), as.character(as.character(linesincorrection)), sep = "")
        path <- "./Results/"
        savelocation <- as.character(paste(as.character(path), as.character(filename), sep = ""))
        write.csv(subset_temp, file = savelocation , quote = TRUE, eol = "\n", na = "NA", row.names = TRUE, fileEncoding = "")
      }}}}
      
###---------------------------- Loading all file names for plotting ----------------------------------------
setwd("E:/WUR/Thesis/R/Wurpi_R_Scripts/Thesis/results") #sets working directory to results folder
filenames = dir(pattern="") # Reads all file into the memory




###------------------------- Plot timeseries ---------------------------------------------------

for (i in 1:length(filenames)) {
  ## read strings as character
  tmp <- read.csv(filenames[i], stringsAsFactors = FALSE)
  ## convert to 'timeDate'
  tmp$tfrm <- timeDate(paste(tmp$cdt, tmp$ctm),format ="%d-%m-%Y") #%H:%M:%S
## create timeSeries object
obj <- timeSeries(as.matrix(tmp$Value), tmp$tfrm)
## store object in the list, by name
lst[[files[i]]] <- as.xts(obj)
}

setwd("E:/WUR/Thesis/R/Wurpi_R_Scripts/Thesis") # Sets the working directory back to the orignal location


tsdata <-NULL
testlocation1 <- "./Results/benchmarkset1"
testlocation2 <- "./Results/benchmarkset2"
testlocation3 <- "./Results/benchmarkset3"
testlocation4 <- "./Results/benchmarkset4"
csv_corrections1 <- read.csv(file=as.character(testlocation1),head=TRUE,sep=",") # Read CSV file into memory
csv_corrections2 <- read.csv(file=as.character(testlocation2),head=TRUE,sep=",") # Read CSV file into memory
csv_corrections3 <- read.csv(file=as.character(testlocation3),head=TRUE,sep=",") # Read CSV file into memory
csv_corrections4 <- read.csv(file=as.character(testlocation4),head=TRUE,sep=",") # Read CSV file into memory
csv_corrections1 <- cbind(csv_corrections1, var2 = as.numeric(gsub(",", ".", gsub("\\.", "", csv_corrections1$Miller))))
plot(csv_corrections1$var2)    
csv_corrections2 <- cbind(csv_corrections2, var2 = as.numeric(gsub(",", ".", gsub("\\.", "", csv_corrections2$Miller))))
plot(csv_corrections2$var2)  
csv_corrections3 <- cbind(csv_corrections3, var2 = as.numeric(gsub(",", ".", gsub("\\.", "", csv_corrections3$Miller))))
plot(csv_corrections3$var2)  
csv_corrections4 <- cbind(csv_corrections4, var2 = as.numeric(gsub(",", ".", gsub("\\.", "", csv_corrections4$Miller))))
plot(csv_corrections4$var2)  
      
plot.ts(x = csv_corrections2$Date, y = csv_corrections2$var2, type = p)      
plot(csv_corrections2$Date, csv_corrections2$var2, type = "p" )       
      #       for(lines1 in 1:nrow(csv_without_corrections)){
plot      
correctionmethod <- 0


    





correctionmethod <- correctionmethod +1
line1 <- 0
c <- NULL
for(lines2 in 1:nrow(csv_without_corrections)){ #number that cycles through the lines
  line1 <- line1 + 1
  print("line1")
  print(line1)
  print("correction")
  print(correctionmethod)
  name_to_filter <- levels(csv_without_corrections$corrections)[correctionmethod]
  print(name_to_filter)
  print(as.character(csv_without_corrections[line1, 5]))
  if(grepl(as.character(name_to_filter), as.character(csv_without_corrections[line1, 5]))){
    b <- csv_without_corrections[line1,]
    c <- rbind(c,b)
    filename <- paste(as.character(settingstring), as.character(correctionmethod), sep = "")
    print(filename)
    path <- "./Results/"
    savelocation <- as.character(paste(as.character(path), as.character(filename), sep = ""))
    print("printed")
    write.csv(c, file = savelocation , quote = TRUE, eol = "\n", na = "NA", row.names = TRUE, fileEncoding = "")

# if(grepl(name_to_filter, csv_without_corrections[line1, 5])){      
#             name_to_filter <- correction[linesincorrection]
#             {
#               
#                 print("start")
#                 print(lines2)
#                 print(linesincorrection)
#                 print(correction[linesincorrection])
#                 print("end")       
#                 b <- settingswithallcorrections[lines2,]
#                 c <- rbind(c,b)
#                 filename <- paste(as.character(settingstring), as.character(linesincorrection), sep = "")
#                 print(filename)
#                 path <- "./Results/"
#                 savelocation <- as.character(paste(as.character(path), as.character(filename), sep = ""))
#               write.csv(c, file = savelocation , quote = TRUE, eol = "\n", na = "NA", row.names = TRUE, fileEncoding = "")
#               }}}
#           
#  
#     }}}}}
# 
# if(grepl(name_to_filter, csv_without_corrections$corrections[lines2])

tsdata <-NULL
testlocation <- "./Results/IS100CV0EV-21"
csv_corrections <- read.csv(file=as.character(testlocation),head=TRUE,sep=",", stringsAsFactors = FALSE) # Read CSV file into memory
# as.xts(csv_corrections)
tsdata <- csv_corrections[,c('Date', 'var2')]#cbind(csv_corrections[3],csv_corrections[22])
tsdata$Date <- strptime(tsdata$Date, format="%d-%m-%Y")
#tsdata$Date[1]<- as.Date(tsdata$Date[1], format="%m/%d/%Y")
class(tsdata$var2)
class(tsdata$Date)


mydate <- factor("1/15/2006 0:00:00")
mydate <- as.Date(mydate, format = "%m/%d/%Y")


data <- tsdata$Date[1]
library(lubridate)
date <- mdy(data)
mydate
class(mydate)


tsdata[order(tsdata$Date ),]
sample_matrix <- data.matrix(tsdata, rownames.force = NA)

plot(dff$var2)
plot(dff$Date, dff$var2)


        
### plot diffent csv's per setting.
for(i in 1:length(m[[1]])){
  IS <- m[[1]][[i]]
  for(y in 1:length(m[[2]])){
    CV <- m[[2]][[y]]
    for(z in 1:length(m[[3]])){
      EV <- m[[3]][[z]]
      temp1 <- paste("IS", as.character(IS), sep = "")
      temp2 <- paste(as.character(temp1),"CV", sep = "")
      temp3 <- paste(as.character(temp2), as.character(CV), sep = "")
      temp4 <- paste(as.character(temp3), as.character("EV"), sep = "")
      settingstring <- paste(temp4, as.character(EV), sep = "")
      dff <- read.csv(file=as.character(settingstring),head=TRUE,sep=",") # Read CSV file into memory
      plot(dff$var2)
      plot(dff$Date, dff$var2, main = paste("Settings:", as.character(settingstring), sep = " "),
           xlab="date", ylab="LAI") #zoo (strucchange) or xts 
    }}}



# dff <- 1
plot(dff$var2)
plot(dff$Date, dff$var2)



# with(df, plot(Date, var2))

heisenberg$Lang

checkif <- sapply(heisenberg$picture.files, grepl, settingstring)

# testbatch$Date <- as.Date(testbatch$Date, "%d/%m/%Y")
# plot(Miller ~ Date, testbatch, xaxt = "n", type = "l")
# axis(1, testbatc$Date, format(testbatc$Date, "%b %d"), cex.axis = .7) 


plot(dff$var2)
plot(dff$Date, dff$var2, main = paste("Settings:", as.character(settingstring), sep = " "),
     xlab="date", ylab="LAI") #zoo (strucchange) or xts 
  }}}



# dff <- 1
plot(dff$var2)
plot(dff$Date, dff$var2)

# if(grepl(name_to_filter, csv_without_corrections[line1, 5])){      
#             name_to_filter <- correction[linesincorrection]
#             {
#               
#                 print("start")
#                 print(lines2)
#                 print(linesincorrection)
#                 print(correction[linesincorrection])
#                 print("end")       
#                 b <- settingswithallcorrections[lines2,]
#                 c <- rbind(c,b)
#                 filename <- paste(as.character(settingstring), as.character(linesincorrection), sep = "")
#                 print(filename)
#                 path <- "./Results/"
#                 savelocation <- as.character(paste(as.character(path), as.character(filename), sep = ""))
#               write.csv(c, file = savelocation , quote = TRUE, eol = "\n", na = "NA", row.names = TRUE, fileEncoding = "")
#               }}}
#           
#  
#     }}}}}
# 
# if(grepl(name_to_filter, csv_without_corrections$corrections[lines2])

tsdata <-NULL
testlocation <- "./Results/IS100CV0EV-21"
csv_corrections <- read.csv(file=as.character(testlocation),head=TRUE,sep=",", stringsAsFactors = FALSE) # Read CSV file into memory
# as.xts(csv_corrections)
tsdata <- csv_corrections[,c('Date', 'var2')]#cbind(csv_corrections[3],csv_corrections[22])
tsdata$Date <- strptime(tsdata$Date, format="%d-%m-%Y")
#tsdata$Date[1]<- as.Date(tsdata$Date[1], format="%m/%d/%Y")
class(tsdata$var2)
class(tsdata$Date)

