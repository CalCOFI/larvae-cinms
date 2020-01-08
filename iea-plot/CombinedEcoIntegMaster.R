### The code should allow you to enter ONE "*.csv" file and produce all the graphs for each section.
### There are a number of choices to make at the beginnig.  Otherwise, everything else should run solo.
### It assumes that the data (csv file), figures, and R functions are all in the same folder.  
### column order should be:
# Year 
# Y data
# how the y data are measured.  The will become the Yaxis lable.  Eg "Biomass" or "Number"
# time series name.  eg Lingcod.  This will become the figure title
# order.  currently non functional.  don't included it.  It is in the example data base however.


#############################################################
####### BEGIN DATA ENTRY ####################################
#############################################################

##### enter information here.#### 
setwd("C:/Users/art/Desktop/diversity/nickfiles")



###########################
data.file = "CombinedEcosystemIntegrity.csv"  # your data. see the included example file for format
X = "year"		# year = x data for the time series plots.  
Y = "index"		# annual mean = y data. same rules
Ylabel = "metric"	# Y axis label based on how the mean is measured (eg biomass or number)
TimeSeries = "time.series.2"	# time series name, eg Lingcod
ComputerType ="PC"	# Computer type Mac or PC.  there is different coding for each.
QuadPlotTitle = "Indicators of Ecosystem Integrity"	# title for the final quadplot
NormalizedData ="NormalizedData.csv"				# name of the file for the normalized data.  Include suffix.
QuadSymbols = c(21,23,21,22,23,24,21,23,21,22,23,24,21,22,23,24,21,22,23,24,21)	# symbols for the quad plot.  
QuadPointSize = 2 							#If you want to use text, use NA (no quotes).  
											#It will use the time series labels for symbols
QuadColors=c('magenta','magenta','red','red','red','red','green','green','black','black','black','black','blue','blue','blue','blue','cyan','cyan','cyan','cyan','yellow')# # colors for the symbols on the quad plots.

QuadLegendSize = 0.84 	# size of legend text
QuadLegendPointSize = 1		#size of legend points NOT graph points


PlotTogether ="No"		# "Yes" or "No".  This will either plot each time series separately or put five on each page and print multiple pages


# you will need to update the address below if the R files are in a different location from the data file above.
		
source("PlotTimeSeries_MACPC.R")
source("QuadPlot2012.R")
source("NormalizeTimeSeries.R")
source("RunAllThree.R") 