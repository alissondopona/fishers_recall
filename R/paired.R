# ----------------------------------------------------------------------
# Authors: Msc. Alisson P. B. Dopona & Ph.D Antônio Olinto Ávila da Silva
# Date: 14/07/2026
# Data analysis of fishers' recall - Master project
# ----------------------------------------------------------------------

# ----------------------------------------- Install and/or load libraries
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

layout2 <- 
  theme_test() +
  theme(
    text=element_text(family="Arial"),
    axis.title.y=element_blank(),
    axis.title.x=element_text(size=22),
    axis.text=element_text(size=22),
    legend.title=element_blank(),
    legend.text=element_text(size=20),
    legend.position = "right", # right, left, bottom, top, none
    plot.title = element_text(size=25,face="bold"),
    strip.text = element_text(size = 18)
  )

# ----------------------------------------- Load the database
dat <- read.csv("Data/dataset.csv", 
                stringsAsFactors = T)
glimpse(dat)

# ----------------------------------------- Organize the database
dat$period <- as.factor(dat$period)
levels(dat$period) <- c("First period", "Second period", "Third period")

dat$period <- factor(dat$period, 
                     levels = c("First period", "Second period", "Third period"))

# ----------------------------------------- Perform statistical analyses
# ---------- Organizing database
dat2 <- tidyr::pivot_wider(dat,
                    names_from = source,
                    values_from = lpue)
glimpse(dat2)

# ---------- Wilcoxon test
stat_wilc <- dat2 |>
  group_by(county, period) |>
  group_modify(~ broom::tidy(wilcox.test(.x$FMP,
                                         .x$FR,
                                         paired = TRUE)))

print(stat_wilc)

# ---------- Effect size
dat <- dat[order(dat$vessel_cod, dat$source),]

effect_size <- dat |> 
  group_by(county, period) |> 
  group_modify(~ broom::tidy(wilcoxonPairedR(x = .x$lpue,
                                             g = .x$source,
                                             ci = TRUE)))

print(effect_size)

# ----------------------------------------- Visualize the results 
# ---------- BoxPlot
ggpubr::ggpaired(dat, 
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

ggplot2::ggsave("Output/fig_2.png",
                dpi = 300,
                width = 8.5,
                height = 9)

# ---------- Effect Size

# --- Algumas sugestoes de plot para o tamanho de efeito
effect_size <- effect_size[, c(1,2,3,5)]
effect_size <- tidyr::pivot_wider(effect_size,
                                  names_from = column,
                                  values_from = mean)

# Option 1
bg <- data.frame(start = c(-1.0,-0.5,-0.1,0.1,0.5),
                 end = c(-0.5,-0.1,0.1,0.5,1.0),
                 cor= c ("Large", "Medium", "Small", "Medium", "Large"))

ggplot2::ggplot()+
  geom_rect(data = bg, 
            aes(xmin = start,
                xmax = end,
                ymin = -Inf,
                ymax = Inf,
                fill = cor)) +
  scale_fill_manual(values = alpha(c("red","yellow","green"), 0.15)) +
  geom_point(data = effect_size,
             aes(y = county,
                 x = r,
                 xmin = lower.ci,
                 xmax = upper.ci,
                 shape = period),
             position = position_dodge(width = 0.5,
                                       reverse = TRUE)) +
  geom_pointrange(data = effect_size,
                  position = position_dodge(width = 0.5,
                                            reverse = TRUE),
                  aes(xmin = lower.ci,
                      xmax = upper.ci,
                      x = r,
                      y = county,
                      shape = period), 
                  size = 0.5) +
  scale_y_discrete(limits = rev) +
  layout2

ggplot2::ggsave("Output/fig_3.png",
                dpi = 300,
                width = 7,
                height = 5)

# Option 2
vline <- c(0,0.1,-0.1,0.5,-0.5)

ggplot2::ggplot(effect_size, aes(y = county,
           x = r,
           xmin = lower.ci,
           xmax = upper.ci)) +
  geom_pointrange(position = position_dodge(width = 0.5,
                                            reverse = TRUE),
                  aes(shape = period), 
                  size = 0.5) + 
  scale_y_discrete(limits = rev) +
  geom_vline(xintercept = vline,
             color = "red", 
             linetype = "dashed", 
             cex = 0.5, 
             alpha = 0.2) +
  layout2 
  
ggplot2::ggsave("Output/fig_3_1.png",
                dpi = 300,
                width = 7,
                height = 5)

