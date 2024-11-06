
library(aLBI)
library(dplyr)
library(tidyverse)
data <- iris %>% 
  select(Sepal.Length, Sepal.Width)

LWRelation(data = lenfreq01)  
