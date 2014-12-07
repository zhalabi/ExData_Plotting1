ExamineHouseholdEnergy_GlobalActivePowerPlot2<-function() {
  ##Setup Environment
  rm(list=ls()) ## Clear Memory
  setwd("~/DataPlot_Project1") ##Set Working Directory
  
  ##Download Datafrom the Web
  url<-"https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip" ##Define Data Source
  download.file(url,"datasource.zip") ##Download Data
  unzip("datasource.zip", overwrite=TRUE) ##Unzip Data
  
  filenamePowerConsumption<-"household_power_consumption.txt"
  dataPowerConsumptionAll<-read.table(filenamePowerConsumption,header=T,stringsAsFactors=FALSE ,sep=";") ##Load Data
  dataPowerConsumption<-dataPowerConsumption<-dataPowerConsumptionAll[dataPowerConsumptionAll$Date %in% c("1/2/2007","2/2/2007"),] ##Filter Data
  dataPowerConsumption$Date <- strptime(dataPowerConsumption$Date,"%d/%m/%Y") ## Convert Date column to date type
  ##dataPowerConsumption$Time<-strptime(dataPowerConsumption$Time,"%H:%M:%S")
  ##dataPowerConsumption[,3:8][dataPowerConsumption[,3:8] == "?" ] <- NA
  
  ##Convert energy consumption columns to numeric types
  dataPowerConsumption[,3]<-as.numeric(dataPowerConsumption[,3])
  dataPowerConsumption[,4]<-as.numeric(dataPowerConsumption[,4])
  dataPowerConsumption[,5]<-as.numeric(dataPowerConsumption[,5])
  dataPowerConsumption[,6]<-as.numeric(dataPowerConsumption[,6])
  dataPowerConsumption[,7]<-as.numeric(dataPowerConsumption[,7])
  dataPowerConsumption[,8]<-as.numeric(dataPowerConsumption[,8])
  
  plot2(dataPowerConsumption)
  
}



plot2 <- function(dataPowerConsumption) {
  png(filename="plot2.png", width = 480, height = 480) ##Define Plot File Format
  days<-c("Thu","Fri","Sat") ##Define Axis1 Labels
  g_range<-range(dataPowerConsumption$Global_active_power) ## Define yAxis Range
  plot(dataPowerConsumption$Global_active_power,type="l",axes=F,ann=F) ##Plot
  axis(1, at=seq(0,dim(dataPowerConsumption)[1],1440), lab=days) ##Format Axis1
  axis(2,at=2*0:g_range[2]) ##Format Axis2
  title(ylab="Global Active Power") ##Label Plot
  box() ##Draw Box around plot
  dev.off() ## Close Graphic Device
}