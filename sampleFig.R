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

t <- seq(0,1,length=100)
x1 <- exp(-1*t)*sin(6*pi*t)
x2 <- exp(-1*t)
df <- data.frame(t=t,x1=x1,x2=x2)
df %>% gather(key = variable, value = value, -t)->d

g <- ggplot(d,aes(x=t, y=value,colour = variable,linetype = variable))
g <- g + theme_bw()
g <- g + geom_line(size = 0.5)
g <- g + xlab("time [s]")
g <- g + ylab("Displacemant [mm]")
g <- g  + theme(title = element_text(size=24)) # enlarge title font
g <- g  + theme(text = element_text(size=21)) # enlarge text font
g <- g + theme(legend.position=c(0.85,0.85),
               legend.background = element_rect(fill="gray95", size=.5, linetype="dotted"));
g <- g + scale_colour_brewer(palette = "Dark2")
plot(g)
ggsave(file = "sample.pdf", plot = g, width = 800/72, height = 400/72) # export figure
