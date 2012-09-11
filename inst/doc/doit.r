### Do Stuff
out <- suppressMessages(llply(c(20, 40, 60, 80, 100, 140, 160), simbaltrees, .progress="text"))
outdf <- ldply(out)
str(outdf)
outdf_melt <- melt(outdf, id.vars=c(1,5,6))

outdf_melt_ <- ddply(outdf_melt, .(type, model, numsp, variable), summarise, 
										 mean = mean(value, na.rm=T),
										 se = sd(value, na.rm=T)/sqrt(na.omit(length(value)))
)

##### Visualize stuff
outdf_melt_ <- read.csv("/Users/ScottMac/Downloads/rstudioec2_outdf_melt_.csv")
library(ggplot2)
ggplot(outdf_melt_, aes(numsp, mean, colour = type)) +
	geom_point(size=4, alpha=0.5) +
	scale_colour_manual(values=c(11,20)) +
	theme_bw() +
	facet_grid(variable ~ model, scales="free")

################## Simulate trees using function simbal
## on Amazon EC2, go to: ec2-50-112-211-18.us-west-2.compute.amazonaws.com
### authenticate with username: rstudio, password is login for this computer

# library(sacbox)
# install.packages(c("plyr","ggplot2","ape","apTreeshape","bipartite","reshape2"))
# library(plyr); library(ape); library(apTreeshape); library(bipartite); library(ggplot2); library(reshape2)