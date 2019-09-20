library(tidyverse)
library(extrafont)
ttf_import("./syntax/fonts")

min_wage <- readxl::read_excel("./data/min_wage_extract.xlsx")
min_wage_ex <- min_wage %>% 
  select(Measure, All) %>% 
  filter(Measure !="Sample Size") %>% 
  mutate(Measure = factor(Measure)) %>%
  mutate(Position = factor(case_when(
    Measure == "The minimum wage should immediately be raised to $15 per hour" ~ "$15 (immediate)",
    Measure == "The minimum wage should gradually be raised to $15 per hour" ~ "$15 (gradual)",
    Measure == "The minimum wage should be raised to an amount under $15 per hour" ~ "$7.25 - $15",
    Measure == "The minimum wage should be kept the same" ~ "$7.25",
    Measure == "The minimum wage should be reduced"  ~ "$0 - $7.25",
    Measure == "The minimum wage should be eliminated" ~ "$0"
  ), levels = c("$15 (immediate)", "$15 (gradual)", "$7.25 - $15","$7.25", "$0 - $7.25", "$0"), ordered=T),
  Color = factor(case_when(
    Position == "$15 (gradual)" ~ 1,
    Position == "$7.25" ~ 2,
    TRUE ~ 0
  )))

ggplot(min_wage_ex, aes(x=Position, y = All, fill = Color)) + geom_col() +
  theme_minimal(base_family = "Quattrocento") + 
  xlab("Preferred Minimum Wage") +
  scale_fill_manual(values=c("2" = "#458490", "1" = "#342c5c", "0" = "#cbd3a3")) +
  scale_y_continuous("", limits = c(0, 375), labels = NULL) +
  geom_segment(aes(x = 2, y = 320, xend = 2.25, yend=350), arrow = arrow(angle = 30, length = unit(0.03, "npc"), ends = "first"), lwd = 1) + 
  annotate("text", x = 3, y = 370, label = "Median Preferred\nMinimum Wage", family = "Quattrocento") +
  geom_segment(aes(x = 4, y = 140, xend = 4.25, yend=170), arrow = arrow(angle = 30, length = unit(0.03, "npc"), ends = "first"), lwd = 1) + 
  annotate("text", x = 5, y = 190, label = "Current Federal\nMinimum Wage", family = "Quattrocento") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")
