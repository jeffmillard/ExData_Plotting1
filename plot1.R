## =============================================================================
## Function:	plot1.R function(state, outcome)
## -----------------------------------------------------------------------------
# Needs library(data.table)

plot1 <- function() {

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

	# for this histogram we do not have to deal with the date/time so 
	# no further prep on data table DT.  But if/when we do want datetime
	#	DT$DateTime <- as.POSIXct(paste(DT$Date,DT$Time,sep=" "))

	# make histogram; set color red, main title and x label
	hist(DT$Global_active_power, breaks=12, col="red", 
     			main="Global Active Power", 
     			xlab="Global Active Power (kilowatts)")

	# make png file
	png(filename = "plot1.png",
			width = 480, height = 480, units = "px", pointsize = 12,
			bg = "white",  type = "quartz")

	dev.off()


}