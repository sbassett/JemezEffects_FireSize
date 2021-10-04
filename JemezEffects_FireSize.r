require("scales")
library(ggplot2)

# load fire size list data
FSIM_FireSizeListsAll_WithHeaders <- read.csv("I:/Projects/JemezTreatmentEffectiveness/Data/Interim/FireSimulation/FSimRuns/FSim_ProcessedManually_2021/FSIM_FireSizeListsAll_WithHeaders.csv", stringsAsFactors=TRUE)

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
