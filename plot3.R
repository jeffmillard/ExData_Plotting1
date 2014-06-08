## =============================================================================
## Function:	plot1.R function(state, outcome)
## -----------------------------------------------------------------------------
# Needs library(data.table)

plot3 <- function() {
	
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

	png(filename = "plot3.png",
	    width = 480, height = 480, units = "px", pointsize = 12,
	    bg = "white",  type = "quartz")

	# make first part of plot (Sub metering 1)
	plot(DT$DateTime,DT$Sub_metering_1, type="l", 
		ylab="Energy sub metering", xlab="")

	# overlay Submetering 2 in red
	lines(DT$DateTime,DT$Sub_metering_2, col="red")

	# overlay Submetering 3 in blue
	lines(DT$DateTime,DT$Sub_metering_3, col="blue")
	
	# add the legend to the upper right on plot
	legend(x="topright", col=c("black", "blue", "red"), lty=1, 
		 legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
	
	# done with the plot; output the file by closing graphic device
	dev.off()
	
	paste("File plot3.png created in directory", getwd(), sep=" ")
}