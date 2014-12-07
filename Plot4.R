ExamineHouseholdEnergy_Plot4<-function() {
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
  
  plot4(dataPowerConsumption)
  
}




plot4 <- function(dataPowerConsumption) {
  png(filename="plot4.png", width = 480, height = 480) ##Define Plot File Format
  par(mfrow=c(2,2)) ##Define Layout of Plots 2 per Row
  
  ##Generate Plot 1
  g_range<-range(dataPowerConsumption$Global_active_power)
  plot(dataPowerConsumption$Global_active_power,type="l",axes=F,ann=F)
  axis(1, at=seq(0,dim(dataPowerConsumption)[1],1440), lab=days)
  axis(2,at=2*0:g_range[2])
  title(ylab="Global Active Power (killowats)")
  box()
  
  ##Generate Plot 2
  g_range<-range(dataPowerConsumption$Voltage)
  plot(dataPowerConsumption$Voltage,type="l",axes=F,ann=F)
  axis(1, at=seq(0,dim(dataPowerConsumption)[1],1440), lab=days)
  axis(2,at=2*0:g_range[2])
  title(xlab= "datetime", ylab="Voltage")
  box()
  
  ##Generate Plot 3
  days<-c("Thu","Fri","Sat")
  plot_colors <- c("black","red","blue")
  g_range <- range(0, dataPowerConsumption$Sub_metering_1,dataPowerConsumption$Sub_metering_2,dataPowerConsumption$Sub_metering_3)
  plot(dataPowerConsumption$Sub_metering_1,type="l",col=plot_colors[1],axes=F,ann=F)
  lines(dataPowerConsumption$Sub_metering_2, type="l", col=plot_colors[2])
  lines(dataPowerConsumption$Sub_metering_3, type="l", col=plot_colors[3])
  axis(1, at=seq(0,dim(dataPowerConsumption)[1],1440), lab=days)
  axis(2,at=10*0:g_range[2])
  legend("topright", g_range[2], c("Sub metering_1","Sub metering_2","Sub metering_3"),cex=0.8, col=plot_colors, lty=1)
  title(ylab="Energy Sub metering")
  box()
  
  ##Generate Plot 4
  g_range<-range(dataPowerConsumption$Global_reactive_power)
  plot(dataPowerConsumption$Global_reactive_power,type="l",axes=F,ann=F)
  axis(1, at=seq(0,dim(dataPowerConsumption)[1],1440), lab=days)
  axis(2,at=seq(0,g_range[2],0.1))
  title(xlab= "datetime", ylab="Global_reactive_power")
  box()
  
  dev.off()
  
}