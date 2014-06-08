## =============================================================================
## Function:	plot4.R function()
## -----------------------------------------------------------------------------
# Needs library(data.table)

plot4 <- function() {
	
	# See plot1.R for detailed comments, not repeated here
	require(data.table)
	
	# -----------------------------------------------------------------
	#                  READ AND FORMAT THE DATA
	# -----------------------------------------------------------------	

	DTcolClasses <- c(rep("character",9))
	
	DT <- fread("household_power_consumption.txt", header=TRUE, na.strings="?",
			sep=";",
			colClasses = DTcolClasses)[Date == "1/2/2007" | Date == "2/2/2007",]
	
	DT <- transform(DT, Date = as.IDate(Date, format="%d/%m/%Y"),
			    Time = as.ITime(Time, format = "%H:%M:%S"),
			    Global_active_power = as.numeric(Global_active_power),
			    Global_reactive_power = as.numeric(Global_reactive_power),
			    Voltage	= as.numeric(Voltage),
			    Global_intensity = as.numeric(Global_intensity),
			    Sub_metering_1 = as.numeric(Sub_metering_1),
			    Sub_metering_2 = as.numeric(Sub_metering_2),
			    Sub_metering_3 = as.numeric(Sub_metering_3))

	DT$DateTime <- as.POSIXct(paste(DT$Date,DT$Time,sep=" "))

	# -----------------------------------------------------------------
	#                GENERATE THE PNG PLOT FILE
	# -----------------------------------------------------------------
	# Open up the png graphics device and start plotting!
	
	png(filename = "plot4.png",
	    width = 480, height = 480, units = "px", pointsize = 12,
	    bg = "white",  type = "quartz")

	# 2 x 2 plots, rowwise
	par(mfrow = c(2,2))

	# --------------------------------------------------------------
	# 1. top left plot (same as plot2)
	# --------------------------------------------------------------
	# make line plot (type="l") of Global_active_power vs DateTime
	plot(DT$DateTime,DT$Global_active_power, type="l", 
     		ylab="Global Active Power (kilowatts)", xlab="")

	# --------------------------------------------------------------
	# 2. top right plot (Voltage vs time)
	# --------------------------------------------------------------
	plot(DT$DateTime,DT$Voltage, type="l", 
     		ylab="Voltage", xlab="datetime")

	# --------------------------------------------------------------
	# 3. bottom left plot (almost!! the same as plot 3)
	# --------------------------------------------------------------
	# Differences :
	#	no box around legend
	#	legend needs rescaling

	# make first part of plot (Sub metering 1)
	plot(DT$DateTime,DT$Sub_metering_1, type="l", 
		ylab="Energy sub metering", xlab="")

	# overlay Submetering 2 in red
	lines(DT$DateTime,DT$Sub_metering_2, col="red")

	# overlay Submetering 3 in blue
	lines(DT$DateTime,DT$Sub_metering_3, col="blue")
	
	# add the legend to the upper right on plot
	legend(x="topright", col=c("black", "blue", "red"), lty=1, 
		 legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
		 bty="n", cex=0.6)

	# --------------------------------------------------------------
	# bottom right plot (Global reactive Power vs time)
	# --------------------------------------------------------------
	plot(DT$DateTime,DT$Global_reactive_power, type="l", 
     		ylab="Global_reactive_power", xlab="datetime")

	# done with the plot; output the file by closing graphic device
	dev.off()
	
	paste("File plot4.png created in directory", getwd(), sep=" ")
}