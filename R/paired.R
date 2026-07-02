# Author: Alisson P. B. Dopona


# Load libraries 
library(tidyverse)

# Load dataset
dat <- read.csv("dat_paired.csv", stringsAsFactors = T)
glimpse(dat)

# Organize dataset
dat$period <- as.factor(dat$period)
levels(dat$period) <- c("First period", "Second period", "Third period")

dat$period <- factor(dat$period, levels = c("First period", "Second period", "Third period"))


# Plot 
ggpaired(dat[dat$ref=="Typ",], x = "source", y = "lpue", id = "vessel_cod", line.color = "gray", line.size = 0.4, linetype = "dashed",) + theme_bw() +
  stat_compare_means(paired = TRUE) +
  labs(y="lpue (kg/day)") +
  theme(axis.title.x=element_blank()) +
  facet_grid(county~period)



head(dat)

dat_typ <- dat |> filter(ref == "Typ")

glimpse(dat_typ)


write_csv(dat_typ, "dataset.csv")
