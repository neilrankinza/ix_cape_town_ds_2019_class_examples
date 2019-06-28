# METADATA ====
# Description: My first R script
# Created: 2019-06-28 (Neil Rankin)
# Last updated: 2019-06-28 (Neil Rankin)
# Last reviewed: NA

# SUMMARY: An introduction following https://r4ds.had.co.nz/data-visualisation.html


# INITIALISE ====


#> setup environment ----
options(digits=2)

#> additional libraries ----

library(tidyverse)

# LOAD DATA ====
mpg

# look at the and discuss types and objects


# My first plot!
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))


# Do exercise 3.2.4 (and circulate to make sure everyone can get this far)


# Work through to
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

# Stretch goal - rest of chapter



