# Run MasterFile to run this and the other files.

#######
data01 = data.frame(read.table(data.file, sep=",", header=TRUE))

#get some infor for plotting etc

Names <- colnames(data01)
no.levels = length(levels(data01[,match(TimeSeries,Names)]))
no.pages = ceiling(no.levels/5)							

#Plot the individual time series Together
if(PlotTogether=="Yes"){
for(k in 1:no.pages){
graphics.off()  													# close any previous graphics

i.range = c(k*5-4,ifelse(k*5<=no.levels,k*5,no.levels))
no.figs <- i.range[2]-i.range[1]+1

jpeg(paste("MyTSPlots_",k,".jpg",sep=""), units="in", res=300, width=7, height=no.figs*2)  		# save the figure
par(mfrow=c(no.figs,1), mar=c(2,5,2,1), oma=c(2,0,0,0))
for(i in i.range[1]:i.range[2]){
	#select one time series and start plotting
	data02 <- subset(data01,data01[,match(TimeSeries,Names)]==levels(data01[,match(TimeSeries,Names)])[i]) 	
	#get various parameters (x, y, etc)
	x<-data02[,match(X,Names)] 
	y <- data02[,match(Y,Names)]
	Ylab <- data02[,match(Ylabel,Names)][1]
	timeseries <- data02[,match(TimeSeries,Names)][1]
	PlotTimeSeries(x,y, Ylab,timeseries, ComputerType)	
	}
mtext("Year", outer=TRUE, side=1, line=1)
dev.off() 															# save the figure

}}


## plot individual time series separately

if(PlotTogether=="No"){
	
	for(i in 1:no.levels){
	select <- levels(data01[,match(TimeSeries,Names)])[i]
	dat.col <- match(TimeSeries,Names)	
	data02 <- subset(data01,data01[,dat.col]==select)
	#data02 <- subset(data01,data01[,match(TimeSeries,Names)]==levels(data01[,match(TimeSeries,Names)])[i]) 	
	#get various parameters (x, y, etc)
	x<-data02[,match(X,Names)] 
	y <- data02[,match(Y,Names)]
	Ylab <- data02[,match(Ylabel,Names)][1]
	timeseries <- data02[,match(TimeSeries,Names)][1]
	graphics.off()
	jpeg(paste(timeseries,".jpg",sep=""), units="in", res=300, width=7.5, height=2.5)
	par(mar=c(4,0,0,0), oma=c(2,3,0,0), pin=c(6.5,1.5))
	PlotTimeSeries(x,y, Ylab,timeseries, ComputerType)
	mtext(Ylab, side=2, outer=TRUE, line=2)	
	mtext("Year",side=1, outer=TRUE, line=1)
	dev.off()
	
}}




# normalize the time series and get data for QuadPlots

data.norm <- data.frame(array(NA,dim=c(1,6)))				# make a data file
colnames(data.norm) <- c("TimeSeries", "ltMean", "ltSD","mn5", "slope", "diff")

for(i in 1:no.levels){
	#seletc one time series to normalize
	select <- levels(data01[,match(TimeSeries,Names)])[i]
	dat.col <- match(TimeSeries,Names)	
	data03 <- subset(data01,data01[,dat.col]==select)
	#data03 <- subset(data01, data01[,match(TimeSeries,Names)]==levels(data01[,match(TimeSeries,Names)])[i]) 	
	x <- data03[,match(X,Names)] 
	y <- data03[,match(Y,Names)]
	timeseries<-data03[,match(TimeSeries,Names)][1] 
	results <- GetNorm(x,y,timeseries)
	colnames(results) <- c("TimeSeries", "ltMean", "ltSD","mn5", "slope", "diff")
	#data.norm[i,] <- results
	data.norm <- data.frame(rbind(data.norm,results))
	
}

data.norm1 <- data.norm[-1,]
data.norm1
#write
write.table(data.norm1,NormalizedData,sep=",", row.names=FALSE, col.names=TRUE)

graphics.off()  # close previous graphics to reset par

jpeg(paste(QuadPlotTitle,".jpg", sep=""), units="in", res=300, width=7, height=7)
QuadPlot(x=data.norm1$diff, y=data.norm1$mn5, Label=data.norm1$TimeSeries,Title=QuadPlotTitle, Symbol=QuadSymbols,Color=QuadColors, LegendCex=QuadLegendSize, LegendPointCex=QuadLegendPointSize, PointCex=QuadPointSize)
dev.off()	




