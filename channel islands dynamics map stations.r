setwd("R:/NOAA projects/CINMS")

library(ggplot2)
library(reshape2)
library(ggmap)

cast1 <- read.csv("updated 1804 1601 1704 1604 1501 1407 1311 ichthyoplankton by line and station.csv", header=TRUE)[,-1]
head(cast1)
unique(cast1$S_C)
cast1$line.station <- with(cast1, paste(S_L,S_S))

cast1$SB <- ifelse(cast1$line.station %in% c("80 55","80 51","81.8 46.9","83.3 55","83.3 51","83.3 42","83.3 40.6"), "Santa Barbara", "Not Santa Barbara")

### prior to 1977 data were collected with a ring net; 1978-presernt with a bongo net.  Adjustements have to be made to make data comparable.
cast2 <- subset(cast1, year > 1977 & season %in% c("spring"))





library(PBSmapping)

 ##########################################################################################################################################################################
############ Get background maps ###################################################################################
### download map data at http://www.soest.hawaii.edu/wessel/gshhs/   select the binary files and save them in your folder####
##### Our goal here is to plot shoreline maps that will be used in subseqent parts of this presentation #############

## importGSHHS is a function from PBSmapping
#### xlim and ylim define the boundaries of the map you want;  longitude has to be in units between 0 and 360.  For negative values you have to add 360
########  
borders <- importGSHHS("R:\\NOAA projects\\gshhg-bin-2.3.4\\wdb_borders_f.b" , xlim= c(-126+360,-114+360), ylim = c(28,37) , maxLevel=1)
rivers <- importGSHHS("R:\\NOAA projects\\gshhg-bin-2.3.4\\wdb_rivers_f.b" , xlim= c(-126+360,-114+360), ylim = c(28,37) , maxLevel=1)
poly1 <- importGSHHS("R:\\NOAA projects\\gshhg-bin-2.3.4\\gshhs_f.b" , xlim= c(-126+360,-114+360), ylim = c(28,37) , maxLevel=1)

#fortify turns the maps files into a data frame.  It's from ggplot2
shore <- fortify(poly1)
border <- fortify(borders)
river <- fortify(rivers)

head(shore)

## Set boundaries of the map
xlim <- c(-122,-115)
ylim <- c(31,36)


#### make a map of all calcofi stations highlighting those near the CINMS

map1 <- subset(cast1, line.station %in% c("76.7 49", "76.7 51", "76.7 55", "76.7 60","80 51", "80 55", 
						"80 60","81.8 46.9","83.3 40.6", "83.3 42", "83.3 51", "83.3 55", "83.3 60",
							"86.7 33", "86.7 35", "86.7 40", "86.7 45", "86.7 50", "86.7 55","86.7 60","90 28", "90 30", "90 35",
							"90 37", "90 45", "90 53","90 60", "93.3 26.7", "93.3 28", "93.3 30", "93.3 35", "93.3 40",
							"93.3 45", "93.3 50","93.3 60"))
head(map1)

 p1 <- ggplot()
	p1 +   
 	coord_map(projection="mercator",xlim=xlim, ylim=ylim) + ## tells it a map is coming; keeps x and y values on same scale; xlim and ylim define the boundaries of the plot
	geom_polygon(aes(x=X, y=Y, group=PID), data=shore, colour='black', fill='grey') +	## adds the map polygons
	geom_path(aes(x=X, y=Y, group=PID), data=river, colour='blue') +  ## adds the river lines
	geom_path(aes(x=X, y=Y, group=PID), data=border, colour='brown') + ## adds the border lines
	geom_point(data=map1, aes(x=longitude, y=latitude, colour=SB), size=2) + 
	scale_colour_manual(values=c("yellow","red")) +
	#geom_text(data=map1, aes(x=longitude, y=latitude, label=line.station), size=3, colour="white") + 
	theme_bw() +		
	labs(list(title="Core CalCOFI points", x="longitude",y="latitude")) +
	scale_x_continuous(breaks = seq(-130,-115,1)) +
	scale_y_continuous(breaks = seq(32,50,1)) +
			theme(
		panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank(), 
	    axis.text.x=element_text(size=12, angle=0), 
		axis.text.y=element_text(size=12, angle=0), 
		axis.title.x=element_text(size=15, angle=0), 
		axis.title.y=element_text(size=15, angle=90),
		strip.background = element_rect(fill='white'), 
		strip.text.x=element_text(size=15),
		legend.position="none",
		legend.text=element_blank(),
		legend.title=element_blank()
		)
ggsave(filename = "calcofi map1 all points.png",width=13.3,height=9.8)



head(map1)
data.frame(colnames(map1))
map2 <- melt(map1, c(108,109), c(5,6))
head(map2)
map3 <- dcast(map2, line.station + SB ~ variable, mean)
head(map3)

 p1 <- ggplot()
	p1 +   
 	coord_map(projection="mercator",xlim=xlim, ylim=ylim) + ## tells it a map is coming; keeps x and y values on same scale; xlim and ylim define the boundaries of the plot
	geom_polygon(aes(x=X, y=Y, group=PID), data=shore, colour='black', fill='grey') +	## adds the map polygons
	geom_path(aes(x=X, y=Y, group=PID), data=river, colour='blue') +  ## adds the river lines
	geom_path(aes(x=X, y=Y, group=PID), data=border, colour='brown') + ## adds the border lines
	geom_point(data=map3, aes(x=longitude, y=latitude, colour=SB), size=2) + 
	scale_colour_manual(values=c("yellow","red")) +
	geom_text(aes(x=longitude, y=latitude, label=line.station), data=map3, colour='blue', size=4, vjust=1)+ ### show where sample points are located
	#geom_text(data=map1, aes(x=longitude, y=latitude, label=line.station), size=3, colour="white") + 
	theme_bw() +		
	labs(list(title="Core CalCOFI points", x="longitude",y="latitude")) +
	scale_x_continuous(breaks = seq(-130,-115,1)) +
	scale_y_continuous(breaks = seq(32,50,1)) +
			theme(
		panel.grid.major = element_blank(), 
		panel.grid.minor = element_blank(), 
	    axis.text.x=element_text(size=12, angle=0), 
		axis.text.y=element_text(size=12, angle=0), 
		axis.title.x=element_text(size=15, angle=0), 
		axis.title.y=element_text(size=15, angle=90),
		strip.background = element_rect(fill='white'), 
		strip.text.x=element_text(size=15),
		legend.position="none",
		legend.text=element_blank(),
		legend.title=element_blank()
		)
ggsave(filename = "calcofi map2 1 point per station.png",width=13.3,height=9.8)


















