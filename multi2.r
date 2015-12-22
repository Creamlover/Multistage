data<-read.csv("R73m.csv")
x<-data$Day
y<-data$Sum
max_x<-length(x)
max_y<-length(y)
RSS_array<-c(1:max_x)
RSS_array<-RSS_array-RSS_array
AIC_array<-c(1:max_x)
AIC_array<-AIC_array-AIC_array
AIC_array_g<-c(1:max_x)
AIC_array_g<-AIC_array_g-AIC_array_g
parameter_array<-c(1:max_x)
parameter_array<-parameter_array-parameter_array
parameterg_array<-c(1:max_x)
parameterg_array<-parameterg_array-parameterg_array
filename<-"data/Part"
data<-cbind(x,y)
laicmin<-0
laici<-0
#Fix gaicmin<-1000 to gaicmin<-0
gaicmin<-0
gaici<-0
k<-0
k0<-0
sumk<-0
j<-1
k_list <- list()
#plot(data,xlim=c(0,x[max_x]*2), ylim=c(0, y[max_y]*2))
x_axis<-seq(0,x[max_x]*2)
while(sumk<length(x)-2){
  
  #Initialize
  #print(j)
  #print("j start")
	laicmin<-0
	laici<-0
	gaicmin<-0
	gaici<-0	
	tmpfilename<-paste(filename,j)
	tmpfilename<-paste(tmpfilename,".txt")
	tmppictname<-paste(filename,j)
	tmppictname<-paste(tmppictname,".png")
	
	for(i in (3+sumk):max_x){
		
		tmpx<-array(sumk+1:i)
		tmpx<-x[sumk+1:i]
		tmpy<-array(sumk+1:i)
		tmpy<-y[sumk+1:i]
		
		if (j!=1){
			for(l in 1:length(tmpx)){
				tmpx[l]<-tmpx[l]-x[sumk]
				tmpy[l]<-tmpy[l]-y[sumk]
			}
		}
		
		#print(i)
		anslog<-try(anslog<-nls(tmpy~SSlogis(tmpx,A,B,C),algorithm = "port",trace=FALSE,control=nls.control(warnOnly=TRUE)))
		ansgom<-try(ansgom<-nls(tmpy~SSgompertz(tmpx,A,B,C),algorithm = "port",trace=FALSE,control=nls.control(warnOnly=TRUE)))
		#cat("Ans\n")
		if(class(anslog)!="try-error"){
			parameter<-anslog$m$getPars()
			parameter_array[i]=parameter[1]
			result<-summary(anslog)
			predict.c <- predict(anslog,list(tmpx = x_axis))
			resultaic <- AIC(anslog)
			resultrss <- deviance(anslog)

			aicwn <- resultaic / (i+1-sumk)
			AIC_array[i]<-aicwn
			
			
			if(aicwn!=0){
				if(laicmin==0){
					laicmin<-aicwn
					laici<-i
				}
				else if(aicwn<laicmin){
					laicmin<-aicwn
					laici<-i
				}
			}
		}
		
		if(class(ansgom)!="try-error"){
			parameterg<-ansgom$m$getPars()
			parameterg_array[i]=parameterg[1]
			resultg<-summary(ansgom)
			predictg.c <- predict(ansgom,list(tmpx = x_axis))
			resultaicgom <- AIC(ansgom)
			resultrssgom <- deviance(ansgom)

			aicwngom <- resultaicgom / (i+1)
			AIC_array_g[i]<-aicwngom
			
			if(aicwngom!=0){
				if(gaicmin==0){
					gaicmin<-aicwngom
					gaici<-i
				} else if(aicwngom<gaicmin){
					gaicmin<-aicwngom
					gaici<-i
				}
			}
		}
	}
	aicmin<-min(laicmin,gaicmin)
	if(aicmin==laicmin){
	  print("logistic")
	  k<-laici
	  if (j==1){
	  	plotx<-tmpx[1:k]
	  	ploty<-tmpy[1:k]
	  }
	  else
	  {
	  if(k<length(tmpx)) {
		plotx<-x[k0+1:k]
	    ploty<-y[k0+1:k]
	    for(m in 1:length(plotx)){
	      plotx[m]<-plotx[m] - x[k0]
	      ploty[m]<-ploty[m] - y[k0]
	      }
	  } else {
	    plotx<-x[k0+1:k]
	    ploty<-y[k0+1:k]
	    plotx<-plotx[!is.na(plotx)]
	    ploty<-ploty[!is.na(ploty)]
	    for(m in 1:length(plotx)){
	      plotx[m]<-plotx[m] - x[k0]
	      ploty[m]<-ploty[m] - y[k0]
	    }
	  }
	  }
	  plotx_axis<-seq(0,plotx[k]*2)
		tmpdata<-data.frame(plotx,ploty)
		plotanslog<-try(plotanslog<-nls(ploty~SSlogis(plotx,A,B,C),algorithm = "port",trace=FALSE,control=nls.control(warnOnly=TRUE)))
		plotpredict.c <- predict(plotanslog,list(plotx = plotx_axis))
		png(tmppictname)
		plot(tmpdata,xlim=c(0,plotx[k]*2), ylim=c(0, ploty[k]*2), xlab="Day", ylab="# of Issues" )
		lines(plotx_axis,plotpredict.c)
		dev.off()
	}	else {
	  print("Gompertz")
	  k<-gaici
	  
	  if (j==1){
	  	plotx<-tmpx[1:k]
	  	ploty<-tmpy[1:k]
	  }
	  else {
		if(k<length(tmpx)) {
			plotx<-x[k0+1:k]
	   		ploty<-y[k0+1:k]
	    	for(m in 1:length(plotx)){
	      		plotx[m]<-plotx[m] - x[k0]
	      		ploty[m]<-ploty[m] - y[k0]
	    	}
   	  	} else {
      		plotx<-x[k0+1:k]
      		ploty<-y[k0+1:k]
      		plotx<-plotx[!is.na(plotx)]
      		ploty<-ploty[!is.na(ploty)]
      		for(m in 1:length(plotx)){
       			plotx[m]<-plotx[m] - x[k0]
        		ploty[m]<-ploty[m] - y[k0]
      		}
    	}
	  }
		#print(length(plotx))
		#print(k)
		#print(max_x)
		plotx_axis<-seq(0,plotx[length(plotx)]*2)
		tmpdata<-data.frame(plotx,ploty)
		plotansgom<-try(plotansgom<-nls(ploty~SSgompertz(plotx,A,B,C),algorithm = "port",trace=FALSE,control=nls.control(warnOnly=TRUE)))
		plotpredictg.c <- predict(plotansgom,list(plotx = plotx_axis))
		png(tmppictname)
		plot(tmpdata,xlim=c(0,plotx[length(plotx)]*2), ylim=c(0, ploty[length(plotx)]*2), xlab="Day", ylab="# of Issues" )
		lines(plotx_axis,plotpredictg.c)
		dev.off()
	}
	print(laicmin)
	print(gaicmin)
	sumk<-k
	k0<-k
	print("sum k")
	print(x[sumk])
	print("j")
	print(j)
	j<-j+1
}
print("End")