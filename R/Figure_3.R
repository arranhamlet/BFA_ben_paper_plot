library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(lubridate)
library(data.table)
library(ggpubr)

malaria_data <- readRDS("data/counterfactual_all_simulations.rds")
malaria_data$scenario <- gsub("baseline", "Baseline",
                              gsub("current", "Current situation",
                                   gsub("no bednets", "No bednets",
                                        gsub("no smc", "No SMC",
                                             gsub("no acts", "No ACT",
                                                  gsub("no resistance", "No pyrethroid resistance",
                                                       gsub("no outdoor biting", "No outdoor biting",
                                                            gsub("No resistance or outdoor biting", "No resistance or outdoor biting",
                                                                 malaria_data$scenario))))))))


malaria_data$scenario <- factor(malaria_data$scenario,
                                       levels = rev(c("Baseline",
                                                      "Current situation",
                                                      "No bednets",
                                                      "No SMC",
                                                      "No ACT",
                                                      "No pyrethroid resistance",
                                                      "No outdoor biting",
                                                      "No resistance or outdoor biting")))


#Part 1 of figure 3 - change in prevalence over time
#Need to population weight this, but for now will just take the mean
#Hard to tell what is going on with the seasonal dynamics, will take annual mean
malaria_data$year_round <- plyr::round_any(malaria_data$year, 1, f = floor)

#Here we're assuming that each adm1 location has had its prevalence weighted by the proportion
#of the national average (will do this when I get the data)

malaria_seasonal_total <- aggregate(x = list(prev_all = malaria_data$prev_all,
                                         incidence_all = malaria_data$clin_inc_all),
                                by = list(year = malaria_data$year,
                                          scenario = malaria_data$scenario),
                                FUN = mean)

malaria_data_total <- aggregate(x = list(prev_all = malaria_data$prev_all,
                                         incidence_all = malaria_data$clin_inc_all),
                                by = list(year_round = malaria_data$year_round,
                                          scenario = malaria_data$scenario),
                                FUN = mean)

#Plot the mean annual differences
malaria_prevalence_all <- ggplot(data = subset(malaria_data_total, year_round != 2021)) +
  geom_line(data = subset(malaria_seasonal_total, scenario == "Current situation" & year < 2021), 
            aes(x = year, y = prev_all), col = "gray50", alpha = 0.5) +
  geom_line(aes(x = year_round, y = prev_all, color = scenario, group = scenario)) +
  theme_minimal() +
  labs(x = "", y = "Prevalence all ages (%)", color = "") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = T, reverse = T))

malaria_prevalence_all

#Now work out what the difference between baseline and others is
#Probably a simple way of doing this with tidyverse I haven't worked out yet
baseline_difference <- as.data.frame(rbindlist(sapply(unique(malaria_data_total$year_round), function(x){
  
  data_year <- malaria_data_total[which(malaria_data_total$year_round == x), ]
  baseline_data <- data_year[which(data_year$scenario == "Baseline"), ]
  
  rbindlist(sapply(unique(malaria_data_total$scenario), function(y){
    
    scenario_data <- data_year[which(data_year$scenario == y), ]
    
    data.frame(scenario = y,
               year = x,
               value = scenario_data$incidence_all * 100000,
               absolute_difference = baseline_data$incidence_all - scenario_data$incidence_all,
               reduction = 1 - scenario_data$incidence_all/baseline_data$incidence_all,
               baseline_value = baseline_data$incidence_all * 100000,
               midpoint = mean(c(scenario_data$incidence_all * 100000, baseline_data$incidence_all * 100000)),
               stringsAsFactors = FALSE)
    
  }, simplify = FALSE))
  
}, simplify = FALSE)))

baseline_difference$scenario <- factor(baseline_difference$scenario,
                                       levels = rev(c("Baseline",
                                                      "Current situation",
                                                      "No bednets",
                                                      "No SMC",
                                                      "No ACT",
                                                      "No pyrethroid resistance",
                                                      "No outdoor biting",
                                                      "No resistance or outdoor biting")))


barplot_incidence_difference <- ggplot(data = subset(baseline_difference, year == 2018), aes(x = value, y = scenario, fill = scenario)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.75) +
  theme_minimal() +
  labs(x = "Estimated cases per 100,000 per year", y = "") +
  theme(legend.position = "none") +
  geom_segment(data = subset(baseline_difference, year == 2018 & scenario != "Baseline"),
               aes(x = value, 
                   xend = baseline_value, 
                   yend = scenario),
               arrow = arrow(length = unit(0.25, "cm")), size  = 1.25) +
  geom_segment(data = subset(baseline_difference, year == 2018 & scenario != "Baseline"),
               aes(xend = value, 
                   x = baseline_value, 
                   yend = scenario),
               arrow = arrow(length = unit(0.25, "cm")), size  = 1.25) +
  geom_label(data = subset(baseline_difference, year == 2018 & scenario != "Baseline"),
             aes(x = midpoint, y = scenario, label = paste0(round(reduction, 2) * 100, "%")), fill = "white") +
  scale_x_continuous(position = "top", labels = scales::comma, limits = c(0, 100000)) 


ggarrange(malaria_prevalence_all, barplot_incidence_difference, labels = c("A", "B"))

ggsave("figs/BFA_ben_plot.jpg", width = 10, height = 5)



