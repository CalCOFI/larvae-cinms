
QuadPlot<-function(x,y,Label,Title, Symbol, Color,LegendCex,LegendPointCex, PointCex){
		
	
	par(mar=c(7,5,7,9),xpd=NA)
	x1 <- as.numeric(as.character(x))
	y1 <- as.numeric(as.character(y))
	rng = max(ceiling(abs(c(x1,y1))))
	plot(x1,y1, xlim=c(-4,4), ylim=c(-2,2), xlab="Short-term trend", ylab="Long-term trend", type="n", cex=2, cex.lab=2, bty="n")

	#get dimensions for plotting
	xx=par()
	nx = xx$usr[1]
	px = -xx$usr[1]
	ny = xx$usr[3]
	py = xx$usr[4]
	
	# some polygons to color each section differently
	polygon(x=c(0,0,px,px),y=c(py,0,0,py), col="#00FF0055")
	polygon(x=c(0,0,nx,nx),y=c(ny,0,0,ny), col="#FF000055")
	polygon(x=c(0,0,px,px),y=c(ny,0,0,ny), col="#FFFF0055")
	polygon(x=c(0,0,nx,nx),y=c(py,0,0,py), col="#FFFF0055")
	
	
	#add mean and 1 SD lines
	segments(-4,0,4,0,lwd=1)
	segments(0,-2,0,2, lwd=1)
	segments(-rng,1,rng,1, lty="dotted", lwd=2)
	segments(-rng,-1,rng,-1, lty="dotted", lwd=2)
	segments(-1,-2*0.95,-1,2*0.95, lty="dotted", lwd=2)
	segments(1,-2*0.95,1,2*0.95, lty="dotted",  lwd=2)
	
	#re-graph the points so that they are on top
	if(is.na(Symbol[1])==TRUE){text(x1,y1, Label, cex=1.5)}
	if(is.numeric(Symbol[1])==TRUE){
		if(min(Symbol)>20){col.1="black"}else{col.1=Color}}
	if(is.numeric(Symbol[1])==TRUE){points(x,y,pch=Symbol, col=col.1, bg=Color, cex=PointCex)}

	 
	text(rng,rng,"High & increasing", pos=2, font=2)  # upper right
    text(rng,-rng,"Low but increasing", pos=2, font=2)  # lower right
    text(-rng,-rng,"Low & decreasing", pos=4, font=2)  #lower left
    text(-rng,rng,"High but decreasing", pos=4, font=2)
     
    title(Title)
	if(is.numeric(Symbol[1])==TRUE){legend(px,py,Label, pch=Symbol, col=col.1, pt.bg=Color,cex=LegendCex, bty="n", pt.cex=LegendPointCex)}

} #end function


#### EXAMPLE #########




