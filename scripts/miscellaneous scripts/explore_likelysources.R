# Explore the dataset that Andy sent me 1/02/22

#Read in the data
data <- read.csv("data/LikelySources.csv")

#Load up the tidyverse
library(tidyverse)

#Check the data
head(data)
str(data)
summary(data)


#Lots of columns, lets select the columns that look interesting and worth exploring!
need <- 
  data %>%
  select(Region, Sampling_target, Sampling_method, Predominant_habitat, Use_intensity) 

#Check the data
head(need)
str(need)
summary(need)

#Convert all the columns into factors
need_factor <- need %>%
  mutate_if(is.character,as.factor)

#Check to see all columns have been converted to factors
str(need_factor)
summary(need_factor)

#Create some plots!
p_region <- 
  ggplot(data = need_factor, aes(x = Region, colour = Region, fill = Region)) + 
  geom_bar() +
  geom_text(stat = "count", aes(label=..count.., vjust= -1)) # this adds a label of the number of studies for each region on the plot

#ggsave("figures/regions.png")


p_habitat <- 
  ggplot(data = need_factor, aes(x = Predominant_habitat, colour = Predominant_habitat, 
  fill = Predominant_habitat)) + 
  geom_text(stat = "count", aes(label=..count.., hjust= -1)) +
  geom_bar() +
  coord_flip() +
  theme(legend.position = "none") 

#ggsave("figures/habitat.png", width = 11)

p_intensity <- 
  ggplot(data = need_factor, aes(x = Use_intensity, colour = Use_intensity,
  fill = Use_intensity)) + 
  geom_text(stat = "count", aes(label=..count.., vjust= -1)) +
  geom_bar() 

#ggsave("figures/sampling_intensity.png")

p_target <- 
  ggplot(data = need_factor, aes(x = Sampling_target, colour = Sampling_target,
  fill = Sampling_target)) + 
  geom_bar() +
  geom_text(stat = "count", aes(label=..count.., vjust= -1)) 

p_method <- 
  ggplot(data = need_factor, aes(x = Sampling_method, colour = Sampling_method,
  fill = Sampling_method)) + 
  geom_bar() +
  geom_text(stat = "count", aes(label=..count.., hjust= -1)) +
  coord_flip() +
  theme(legend.position = "none") 

#ggsave("figures/sampling_method.png", width = 10)


# Cowplot can be used to plot multiple plots on the same page



