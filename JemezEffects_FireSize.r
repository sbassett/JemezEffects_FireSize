require("scales")
library(ggplot2)

# load fire size list data
FSIM_FireSizeListsAll_WithHeaders <- read.csv("data/FSIM_FireSizeListsAll_WithHeaders.csv", stringsAsFactors=TRUE)

# basic violin plot
plotTest1 <- ggplot(FSIM_FireSizeListsAll_WithHeaders, aes(x=Scenario, y=SizeAc)) + geom_violin(fill="gray", trim=FALSE)

plotTest1

# possible values for trans : 'log2', 'log10','sqrt'
plotTest2 <- plotTest1 + scale_y_continuous(trans='log10')

plotTest2

#plotTest3 <- plotTest2 + stat_summary(fun.data=mean_sdl, mult=1, geom="pointrange", color="red")
plotTest3 <- plotTest2 + geom_boxplot(width=0.1)

#stat_summary(fun.data=mean_sdl, mult=1, geom="pointrange", color="red")

plotTest3


plotTest4 <- plotTest3 + scale_y_continuous(trans=pseudo_log_trans(sigma = 1, base = exp(1)))

plotTest4 + theme_classic() + stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red")


means <- aggregate(SizeAc ~  Scenario, FSIM_FireSizeListsAll_WithHeaders, mean)

plotTest4 + stat_summary(fun=mean, geom="point", shape=18, size=5, color="red", fill="red") + geom_text(data = means, aes(label = scales::comma(SizeAc), y = SizeAc * 0.055))


#jpeg(file="testPlot.jpg", width=1950, height=1200)
#plotTest4 + theme_classic()
#dev.off()

### Annual Area Burned by Large Fires
# Sum of all fire areas divided by 10,000 for each scenario.

totalBurnedArea <- aggregate(SizeAc ~ Scenario, FSIM_FireSizeListsAll_WithHeaders, FUN = sum)
totalBurnedArea

totalBurnedArea$aveAnnualBurnedArea <- totalBurnedArea$SizeAc/10000
totalBurnedArea

### average annual probability of a fire over 100,000 acres 
# Count of fires that grew to over 100,000 acres divided by 10,000 for each scenario.

totalMegafire <- with( FSIM_FireSizeListsAll_WithHeaders[FSIM_FireSizeListsAll_WithHeaders$SizeAc >= 100000,], table(Scenario) )
averageAnnualMegaFires <- totalMegafire/10000
averageAnnualMegaFires

### average fire size
# Mean, median quartiles, etc of fire size for each scenario. [I'd expect to see the top quartile decrease faster than the mean or median.]
fireSizeStats <- aggregate(SizeAc ~ Scenario, FSIM_FireSizeListsAll_WithHeaders, summary)
fireSizeStats

#function to find the mode from https://stackoverflow.com/a/25635740
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

#print the mode for each Scenario
aggregate(SizeAc ~ Scenario, FSIM_FireSizeListsAll_WithHeaders, FUN = Mode)
