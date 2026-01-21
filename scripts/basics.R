# load packages ---------
library(tidyverse)
library(here)
library(skimr)

# read in data ----------
beaches <- read_csv(here("data","sydneybeaches.csv"))

# exploring the data ------
view(beaches)
dim(beaches)
str(beaches)
glimpse(beaches)
head(beaches)
tail(beaches)
summary(beaches)

skim(beaches)
