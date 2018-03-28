library(ggplot2)
# 
# ##### Loading the Data #####
# 
# # Set working path
# path = "/home/flocf/Documents/git/SPL/App/"
# setwd(path)
# 
# # Read in data
# WhiteWine = read.csv("data/winequality-white.csv", sep = ";")
# RedWine   = read.csv("data/winequality-red.csv", sep = ";")
# 
# # Add column 'colour' and merge data.frames
# WhiteWine['colour'] = 'white'
# RedWine['colour']   = 'red'
# 
# wines = rbind(WhiteWine, RedWine)

##### Visualization of Data #####
hist_qual = ggplot(wines, aes(quality, fill = colour)) +
              geom_histogram(aes(y = ..density..), alpha = 0.5, position = 'identity', binwidth = 1) +
              facet_wrap(~colour, nrow=2)

hist_qual

hist_alco = ggplot(wines, aes(alcohol, fill = colour)) +
              geom_histogram(aes(y = ..density..), alpha = 0.5, position = 'identity', binwidth = 0.5)

hist_alco

ggplot(data=wines, aes(quality)) + 
  geom_histogram(aes(y =..density..),
                 binwidth = 1,
                 col="red", 
                 fill="green", 
                 alpha=.2) + 
  geom_density(col=2)