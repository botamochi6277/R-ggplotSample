#install.packages('tidyr')
library(tidyr)
#install.packages('dplyr')
library(dplyr) 
#install.packages("ggplot2", dependencies=TRUE)
library(ggplot2)
#install.packages("grid", dependencies=TRUE)
library(grid)
#install.packages("RColorBrewer", dependencies = TRUE)
library(RColorBrewer)
#install.packages("latex2exp")
library(latex2exp)
cols = brewer.pal(8, "Dark2")
ltys = c("solid","dashed","dotdash","longdash","twodash")


# sample data with noise
n <- 20 # number of cycles
t <- seq(0,n,length=100*n) 
x1 <- sin(2*pi*t) + 0.5*runif(length(t))
x2 <- cos(2*pi*t) + 0.2*runif(length(t))

# reshape data for "apply" function
x1Mat <- matrix(x1,100)
x1Avr <- apply(x1Mat,1,mean)
x1Sd <- apply(x1Mat,1,sd)
x2Mat <- matrix(x2,100)
x2Avr <- apply(x2Mat,1,mean)
x2Sd <- apply(x2Mat,1,sd)

# create data frame for ggplot2
cycle = seq(0,2*pi,length=100)
df <- data.frame(t=cycle,x1=x1Avr, x2=x2Avr)
df %>% gather(variable, value = value, -t)->d

dfe <- data.frame(x1=x1Sd, x2=x2Sd)
dfe %>% gather(variable, value = sd)->de
d <- data.frame(d,sd = de$sd) # Merge data frame of values and that of sd.

myplot <- function(d){
  g <- ggplot(d,aes(x=t, y=value,colour = variable,linetype = variable))
  g <- g + theme_bw()
  g <- g + geom_line(size = 1)
  g <- g + geom_ribbon(
    aes(
      x = t,
      ymin = value - sd,
      ymax = value + sd,
      fill = variable
    ),alpha=0.2,size = 0.1)
  g <- g + xlab("Phase [rad]")
  g <- g + ylab("Signal [V]")
  g <- g  + theme(title = element_text(size=24)) # enlarge title font
  g <- g  + theme(text = element_text(size=21)) # enlarge text font
  g <- g + theme(legend.position=c(0.1,0.85),
                 legend.background = element_rect(fill="gray95", size=.5, linetype="dotted"),
                 legend.title=element_blank())
  g <- g + scale_colour_brewer(palette = "Dark2") + scale_fill_brewer(palette = "Dark2") 
  plot(g)
  return(g)
}

g <- myplot(d)
ggsave(file = "sample-line_with_sd.pdf", plot = g, width = 800/72, height = 400/72) # export figure