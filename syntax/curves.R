library(tidyverse)
library(patchwork)
library(extrafont)
ttf_import("./syntax/fonts")
loadfonts(device = "win")

plot_left <- tibble(x = seq(-4, 4, length.out = 100), xpos=x, group = 0, facet= 1, density = dnorm(x, mean=0, sd=1.5)) %>%
  ggplot(aes(x = xpos, y = density)) + 
  geom_line(lwd=1.5) + 
  scale_x_continuous("Preferences", breaks = c(-4, 4), labels = c("Left", "Right")) + 
  ylab("") + 
  scale_y_continuous(limits = c(0, 0.4), labels = NULL) +
  geom_segment(aes(x = 0, y = 0.275, xend = 0, yend=0.3), arrow = arrow(angle = 30, length = unit(0.03, "npc"), ends = "first"), lwd = 1) + 
  geom_hline(aes(yintercept = 0)) + 
  annotate("text", x = 0, y = 0.33, label = "Electorate\nCenter", family = "Quattrocento") +
  theme_minimal(base_family = "Quattrocento") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_right <- tibble(x = rep(seq(-4, 4, length.out = 100),2),
                     density = dnorm(x, mean=0, sd=1.5), 
                     group = rep(c(-3,3), each=length(x)/2),
                     xpos = x + group,
                     facet=2) %>%
  ggplot(aes(x = xpos, y = density, group = group)) + 
  geom_line(lwd=1.5) + 
  scale_x_continuous("Preferences", breaks = c(-7, 7), labels = c("Left", "Right")) + 
  ylab("") + 
  scale_y_continuous(limits = c(0, 0.4), labels = NULL) +
  geom_hline(aes(yintercept = 0)) + 
  theme_minimal(base_family = "Quattrocento") + 
  geom_segment(aes(x = -2.75, y = 0.275, xend = -1.25, yend=0.3), arrow = arrow(angle = 30, length = unit(0.03, "npc"), ends = "first"), lwd = 1) + 
  geom_segment(aes(x = 2.75, y = 0.275, xend = 1.25, yend=0.3), arrow = arrow(angle = 30, length = unit(0.03, "npc"), ends = "first"), lwd = 1) + 
  annotate("text", x = 0, y = 0.33, label = "Partisan\nElectorate\nCenters", family = "Quattrocento") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

plot_left + plot_right

