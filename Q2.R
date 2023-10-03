rm(list = ls())
### Libraries (if necessary)
library(tidyverse)

set.seed(2023)
load("Q2_Data.RData")

##### Question 2.A

## True Underlying Model
N = 50
x = runif(N, -1, 1)
e = rnorm(N, 0, 1)
y = sin(pi * x) + e

xVal = seq(-1, 1, length = N)
yVal = sin(pi * xVal)

lmPlotData = data.frame("X" = x,
                        "Y" = y,
                        "undX" = xVal,
                        "undY" = yVal)

pdf("modPlot_Q2.pdf")
ggplot(lmPlotData, aes(x = undX)) +
  geom_line(aes(y = undY), color = "black", size = 3, linetype = 1) +
  geom_point(aes(x = X, y = Y), color = "red", size = 3) +
  labs(x = "X", y = "Y") +
  theme_bw(base_size = 16)
dev.off()

save.image("Q2_Data.RData")
