# METADATA ====
# Description: EDA
# Created: 2019-06-30 (Neil Rankin)
# Last updated: 2019-06-30 (Neil Rankin)
# Last reviewed: NA

# SUMMARY: An introduction following https://r4ds.had.co.nz/exploratory-data-analysis.html
# If you want to do some 'background' work follow R4DS, what we'll do in class is apply a similar approach to the EC data



# INITIALISE ====


#> setup environment ----
options(digits=2)


#> additional libraries ----

library(tidyverse)


# FOLLOWING R4DS

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))


