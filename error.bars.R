error.bars <- function(yv, z, nn){
   # Function to plot error bars on a barchart
   # Adapted from Crawley p. 127
   #
   # yv: yvalues
   # z : errors you want to plot for each y value
   # nn: labels for each bar
   
   # To use: source this script to put error.bars function in your workspace
   # yvals <- c(45, 34, 76, 50)
   # errvals <- c(5,7,6,3)
   # labels <- c("treatA", "TreatB", "TreatC", "TreatD")
   # error.bars(yvals, errvals, labels)
    
   xv <- barplot(yv, ylim=c(0, (max(yv)+max(z))),names=nn, ylab=deparse(substitute(yv)))
   g <- (max(xv)-min(xv))/50
   for (i in 1:length(xv)) {
      lines(c(xv[i],xv[i]), c(yv[i]+z[i],yv[i]-z[i]))
      lines(c(xv[i]-g,xv[i]+g), c(yv[i]+z[i], yv[i]+z[i]))
      lines(c(xv[i]-g,xv[i]+g), c(yv[i]-z[i], yv[i]-z[i]))
   }
}


