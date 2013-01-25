yplot <- function(years){
  ## Get data of the year and render plots
  
  par(mfrow = c(length(years), 1))
  
  for(year in years){
    data <- read.csv(sprintf("data/rp5_vladimir%d.csv", year),
                     colClasses = "character", sep = ";")
    data[,1] <- as.POSIXct(data[,1], format = "%d.%m.%Y %H:%M")
    data[,2] <- as.numeric(data[,2])
    
    nt <- data[grep("01:00", data[,1]),]
    dt <- data[grep("13:00", data[,1]),]
    
    plot(nt[,1], nt[,2], type = "l", col = "blue", ylim = c(-30, 40), xaxt = "n",
         main = sprintf("Temperate for %d Year", year),
         xlab = "Months", ylab = "Temperate Rank")
    points(dt[,1], dt[,2], type = "l", col = "red")
    
    #legend(2, 38, legend = c("Night", "Day"), lty = c(1, 1),
    #       lwd = c(2.5, 2.5), col = c("blue", "red"))
    
    ms <- split(data, months(data[,1]))
    axis(1, at = lapply(ms, function(x) x[nrow(x)/2,1]), 
         labels = paste(names(ms), " (", ")"), las = 2, cex.axis = 0.7)
  }
}