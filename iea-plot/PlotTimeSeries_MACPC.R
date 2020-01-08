#### function for plotting time series for the IEA write up. 
# most of the imput is straight forward
# X is the xaxis data usually Year
# Y is the xaxis data (biomass, number, etc)
# Ylab is the label for the Y axis. It should be in quotes, eg, "Biomass"
# Computer Type: Enter "PC" or "Mac".  This command influences how the program finds the correct font to graph the arrows.  

# Mac Users.  If you have problems with the arrows, you may need to move the "Wingdings 3" font out of the Microsoft folder within the main Fonts folder and place it in the main Fonts folder.


# an example with fake data is below--after the funtion.

PlotTimeSeries <-function(X,Y,Ylab, Title, ComputerType){ 
              
       Xmin = min(X)
       Xmax = (max(X)- min(X))*1.1 + min(X)
       
       # plot time series.
       plot(Y~X, xlab=NA, ylab=Ylab, pch=19 , type="l", lty="solid", lwd=3,col="red", xlim=c(Xmin,(Xmax)),bty="n",xaxt="n")
       axis(side=1,at=c(min(X):max(X)))
       title(Title, line=0.5)
                
       # mean and sd of the full time series
       stdev = sd(Y)
       mn = mean(Y)
       Usd = mn+stdev
       Lsd = mn-stdev

                      
       # get last 5 years of data
       X5 = X[(length(X)-4):length(X)]
       Y5 = Y[(length(X)-4):length(X)]
       
       # draw various polygons
       xx=c(X,rev(X))
       yy=c(Y,rep(Usd,length(Y)))
             
       polygon(xx,yy,border=NA,col="grey")
       polygon(x=c(min(X),max(X),max(X),min(X)),y=c(Usd,Usd,Lsd,Lsd),col='white', border=NA)
       polygon(x=c(X5[1],X5[5],X5[5],X5[1]), y=c(Usd,Usd,Lsd,Lsd), border=NA,col='#00FF0055')
       
       #plot longterm mean and sd
       segments(Xmin,mn,max(X),mn, col="darkgreen",lty="dotted",lwd=2)                 #Xmax
       segments(Xmin,Usd,max(X),Usd, col="darkgreen",lwd=2)
       segments(Xmin,Lsd,max(X),Lsd,col="darkgreen",lwd=2) 

       #replot the data on top
       lines(Y~X,lwd=2,col='red')
       points(Y~X, pch=19, cex=0.5 )
                
       #get plot dimensions for scaling
       xx<-par() 
       x1<-xx$usr[1]
       x2<-xx$usr[2]
  
       # run regression on last 5 years
       #check if the change is greater or less than one sd of full time series       
       m1 = lm(Y5~X5)
       s1 <- summary(m1)
       b1 <- s1$coefficients[2,1]
       pval <- s1$coefficients[2,4]
       pred = predict(m1)
       delta = pred[5] - pred[1]
       Z = abs(delta)-(stdev)
       
       #lines(pred~X5, lwd=2, col='blue')   # plot trend last five years
        
       #get some data to plot decoratons
       xx<-par() 
       x1<-xx$usr[1]
       x2<-xx$usr[2]

       x01 <- (x2-x1)*0.92 + x1     	# center of point on xaxis
       y01 <- mn+stdev/2     			# y locations
       y02 <- mn-stdev/2
    
       # plot upper arrow or equals sign
		
       LineWT = 3
       arrow.color = "white"
       afont = 1
       points(x01,y01,pch=19,cex=3.8)
       arrow.cex=2.4
	  

       if(ComputerType=="PC"){windowsFonts(font1=windowsFont("Wingdings 3"));if(Z<=0){text(x01,y01,family="font1","n", cex=arrow.cex, col=arrow.color, font=afont)}else{
       if(delta>0){text(x01,y01,family="font1","k", cex=arrow.cex, col=arrow.color, font=afont)}else{ 
       text(x01,y01,family="font1","m", cex=arrow.cex, col=arrow.color, font=afont)}}}

	if(ComputerType=="Mac"){if(Z<=0){text(x01,y01,family="Wingdings 3","n", cex=arrow.cex, col=arrow.color, font=afont)}else{
       if(delta>0){text(x01,y01,family="Wingdings 3","k", cex=arrow.cex, col=arrow.color, font=afont)}else{ 
       text(x01,y01,family="Wingdings 3","m", cex=arrow.cex, col=arrow.color, font=afont)}}}
       

       # get mean of last five years
       mn5 = mean(Y5)
       dMean = mn - mn5
       Z2 = abs(dMean)-stdev
       
             
       # plot info for 5 year mean
          #set font size for +/-/= signs to follow,
       CEX=2
       if(Z2<=0){points(x01,y02,cex=1, pch=19)}else{points(x01,y02,pch=19,cex=3.8)}
       
       if(Z2<=0){points(x01,y02,cex=1, pch=19)}else{if(dMean>0){text(x01,y02,"-", cex=CEX, col=arrow.color, font=afont)}else{text(x01,y02,"+", cex=CEX, col=arrow.color, font=afont)}}
     

       
}  # end funtion  

#################################################
### example fake data ###########################
#################################################


#X=c(2000:2011)
#X2 = c(1988:2011)
#Y1=c(10000,13000,12000,9000,8050,7430,8450,10000,9100,9992,10001,10205)
#Y2=c(7000,13000,12000,9000,7050,11430,8450,9000,9600,10500,10600,11205,7000,13000,12000,9000,7050,11430,8450,9000,9600,10500,10600,11205)
#Y3=c(10000,13000,12000,9000,8050,7430,8450,10000,9100,7992,7001,6205)
#Y4=c(10000,11000,9000,9500,10100,10000,11450,12000,13100,13992,13001,13205)
#Y5=c(12000,13000,11000,10500,10100,10000,11450,8500,8000,7500,7200,6800)

#graphics.off()  # just to make sure the plotting works right

#jpeg("/Users/tolimierini/Dropbox/IEA2012_NT/SampleFigure.jpg",units="in",res=300,height=10, width=7)  # turn on to save

#par(mfrow=c(6,1),mar=c(2,4,4,1),oma=c(4,0,0,0))  #upper right boxes are squares at these dimensions
#PlotTimeSeries(X,Y1,"Biomass","Spp A", "Mac")
#PlotTimeSeries(X2,Y2,"Biomass","Spp b", "Mac")
#PlotTimeSeries(X,Y3,"Biomass","Spp C" ,"Mac")
#PlotTimeSeries(X,Y4,"Biomass","Spp D","Mac")
#PlotTimeSeries(X,Y5,"Biomass","Spp E","Mac")
#PlotTimeSeries(X,Y5,"Biomass","Spp F","Mac")
#mtext("Year", outer=TRUE, side=1, line =2)
#dev.off() #turn on to save figure

