# ----------------------------------------------------------------------
# Author: Msc. Alisson P. B. Dopona & Ph.D Antonio Olinto Ávila da Silva
#
# Data analysis of fishers' recall - Master project
# ----------------------------------------------------------------------

# --------------------------------- Install and Load libraries 
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(ggpubr)){install.packages("ggpubr")}
if(!require(broom)){install.packages("broom")}
if(!require(rcompanion)){install.packages("rcompanion")}


# ----------------------------------------- Layout
layout <-
  theme_bw() +
  theme(
    text=element_text(family="Arial"),
    axis.title=element_text(size=22),
    axis.title.x=element_blank(),
    axis.text=element_text(size=22),
    legend.title=element_text(size=20),
    legend.text=element_text(size=20),
    legend.position = "bottom", # right, left, bottom, top, none
    plot.title = element_text(size=25,face="bold"),
    strip.text = element_text(size = 18)
  )


# --------------------------------------- Load dataset
dat <- read.csv("Data/dataset.csv", stringsAsFactors = T)
glimpse(dat)

# -------------------------------------- Organize dataset
dat$period <- as.factor(dat$period)
levels(dat$period) <- c("First period", "Second period", "Third period")

dat$period <- factor(dat$period, 
                     levels = c("First period", "Second period", "Third period"))

# -------------------------------------------- Plot 
ggpaired(dat, 
         x = "source", 
         y = "lpue", 
         id = "vessel_cod", 
         line.color = "gray", 
         line.size = 0.4, 
         linetype = "dashed") +
  stat_compare_means(paired = TRUE,
                     label = "p.format",
                     label.x = 1.3,
                     label.y = 350) +
  labs(y = "LPUE (Kg/day)") +
  layout +
  facet_grid(county~period)

ggsave("Output/fig_2.png",
       dpi = 300,
       width = 8.5,
       height = 9)


# ------------------------------------ Statistical analysis

#dat2 <- pivot_wider(dat, names_from = source, values_from = lpue)
#glimpse(dat2)

# Wilcoxon test

stat_wilc <- dat |>
  group_by(county, period) |>
  group_modify(~ broom::tidy(wilcox.test(.x$FMP, .x$FR, paired = TRUE)))

print(stat_wilc)

# Effect size

dat <- dat[order(dat$vessel_cod, dat$source)]

effect_size <- dat |> 
  group_by(county, period) |> 
  group_modify(~ broom::tidy(wilcoxonPairedR(x = .x$lpue,
                  g = .x$source)))

print(effect_size)




