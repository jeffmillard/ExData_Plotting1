## =============================================================================
## Function:	plot1.R function()
## -----------------------------------------------------------------------------

plot1 <- function() {

	# Needs library(data.table) - You need to install package if not 
	# already installed.
	# install.package("data.table")
	require(data.table)
	
	# -----------------------------------------------------------------
	#                  READ AND FORMAT THE DATA
	# -----------------------------------------------------------------	
	# Set all column classes to character to suppress errors/warnings
	# tried using first 2 character and rest numeric but problems with
	# fread and na.strings obliged me to abandon that approach
	
	DTcolClasses <- c(rep("character",9))
	
	# Read file into data table using fread() for speed
	# Used the final expression in square brackets to subset the data as
	# part of the read.  Note, this simply matches strings, not actual dates
	# so it is not elegant, but it works
	
	DT <- fread("household_power_consumption.txt", header=TRUE, na.strings="?",
			sep=";",
			colClasses = DTcolClasses)[Date == "1/2/2007" | Date == "2/2/2007",]
	
	# Note, there is a known bug in fread() where the na.strings doesn't work
	# properly. In this example, it turns out to not matter because the subset
	# does not contain and "?" entries
	
	# Now we have the data but columns 3:9 must be numeric
	DT <- transform(DT, Date = as.IDate(Date, format="%d/%m/%Y"),
			    Time = as.ITime(Time, format = "%H:%M:%S"),
			    Global_active_power = as.numeric(Global_active_power),
			    Global_reactive_power = as.numeric(Global_reactive_power),
			    Voltage	= as.numeric(Voltage),
			    Global_intensity = as.numeric(Global_intensity),
			    Sub_metering_1 = as.numeric(Sub_metering_1),
			    Sub_metering_2 = as.numeric(Sub_metering_2),
			    Sub_metering_3 = as.numeric(Sub_metering_3))
	
	# We need to plot vs sate.time so we do want datetime.  Combine Date and
	# Time character vectors to a POSIXct new column 
	DT$DateTime <- as.POSIXct(paste(DT$Date,DT$Time,sep=" "))
	
	# -----------------------------------------------------------------
	#                GENERATE THE PNG PLOT FILE
	# -----------------------------------------------------------------
	# Open up the png graphics device and start plotting!

	# make png file; reference histogram has 12 bins.
	# Assignment specifies 480x480px.  Some debate over white
	# vs transparent background; I chose white because it should render
	# on all systems.
	
	png(filename = "plot1.png",
			width = 480, height = 480, units = "px", pointsize = 12,
			bg = "white",  type = "quartz")
	
	# make histogram; set color red, main title and x label
	hist(DT$Global_active_power, breaks=12, col="red", 
	     main="Global Active Power", 
	     xlab="Global Active Power (kilowatts)")
	
	# done with the plot; output the file by closing graphic device
	dev.off()
	
	# added this just to remove the cryptic message automatically 
	# echoed to console when dev.off() executes.
	paste("File plot1.png created in directory", getwd(), sep=" ")
}