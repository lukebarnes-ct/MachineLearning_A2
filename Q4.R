rm(list = ls())
### Libraries (if necessary)
library(tidyverse)
library(pracma)
library(colorspace)

### Data

set.seed(2023)

dat = read.table("Collider_Data_2022 2.txt", 
                 h = TRUE, stringsAsFactors = TRUE)
dim(dat)

### creating continuous colour palette
col = c('magenta','white','lightblue')
color.gradient = function(x, colors = col, colsteps = 50){
  
  colpal = colorRampPalette(colors)
  return( colpal(colsteps)[ findInterval(x, seq(min(x), max(x), length = colsteps)) ] )
}

##### Question 4.A
### Plot the coordinates in the feature space and 
### colour-code according to the response.

var = dat %>% 
  rowwise() %>% 
  mutate(response = which.max(c_across(starts_with("Yi1")))) %>%
  ungroup %>%
  mutate(response = factor(response))

var = dat %>% 
  gather(Response, flag, Yi1:Yi3) %>%
  filter(flag == 1) %>%
  select(-flag) %>%
  mutate(Response = as.factor(Response))

clPlotData = data.frame("X1" = dat$X1,
                        "X2" = dat$X2)

colours = c("magenta", "darkblue", "darkorange")

pdf("dataPlot_Q4.pdf")
ggplot(clPlotData, aes(x = X1)) +
  geom_point(aes(x = X1, y = X2), color = colours[var$Response], size = 4) +
  labs(x = "X1", y = "X2") +
  ylim(-4.5, 4.5) +
  theme_bw(base_size = 16)
dev.off()


##### Question 4.B

### Write an R-function which evaluates the soft-max activation function in
### matrix form.

### Z is 3 x N

softMaxMat = function(z){
  
  expZ = exp(z)
  sumZ = colSums(expZ)
  
  sM = expZ / sumZ
  
  return(sM)
}
