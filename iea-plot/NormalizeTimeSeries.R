

GetNorm <- function(X,Y,Name){

	# normalize all data & get mn and stdev
	mn = mean(Y)
	stdev = sd(Y)
	Ynorm = (Y-mn)/stdev

	# get info for last 5 years
	x1=X[c((length(X)-4):length(x))]
	
	y1=Ynorm[(length(X)-4):length(X)]
	
	mn5 = mean(y1)  # mean of the last 5 years = LONGTERM axix for Quadplot
	mod1 = lm(y1~x1)
	s1 <- summary(mod1)
	slope <- s1$coefficients[2,1]
	pred <- predict(mod1)
	diff <- pred[5]-pred[1] # predicted change over the last five years. ## short term axis for Quadplot 
	name1 <- as.character(Name)
	results <- data.frame(cbind(name1, mn,stdev, mn5, slope, diff))
	return(results)
	
	}


	
	