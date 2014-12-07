ExamineHouseholdEnergy_SubmeteringPlot3<-function() {
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
  
  plot3(dataPowerConsumption)
  
}



plot3 <- function(dataPowerConsumption) {
  png(filename="plot3.png", width = 480, height = 480) ##Define Plot File Format
  days<-c("Thu","Fri","Sat") ##Define Axis1 Labels
  plot_colors <- c("black","red","blue") ##Define Line Colors in the Plot
  g_range <- range(0, dataPowerConsumption$Sub_metering_1,dataPowerConsumption$Sub_metering_2,dataPowerConsumption$Sub_metering_3) ## Define yAxis Range
  plot(dataPowerConsumption$Sub_metering_1,type="l",col=plot_colors[1],axes=F,ann=F) ##Plot Sub metering 1
  lines(dataPowerConsumption$Sub_metering_2, type="l", col=plot_colors[2]) ##Plot Sub metering 2
  lines(dataPowerConsumption$Sub_metering_3, type="l", col=plot_colors[3]) ##Plot Sub metering 3
  axis(1, at=seq(0,dim(dataPowerConsumption)[1],1440), lab=days) ##Format Axis1
  axis(2,at=10*0:g_range[2]) ##Format Axis2
  legend("topright", g_range[2], c("Sub metering_1","Sub metering_2","Sub metering_3"),cex=0.8, col=plot_colors, lty=1) ##Add legends
  title(ylab="Energy Sub metering") ## Add Plot Title 
  box() ##Draw Box around plot
  dev.off() ## Close Graphic Device
}