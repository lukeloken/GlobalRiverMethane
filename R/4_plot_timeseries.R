

library(tidyverse)
library(ggplot2)
library(lubridate)

load(file.path(path_to_dropbox, "db_processingR", 
               "MethDB_tables_converted.rda"))


conc_ts <- conc_df %>%
  filter(!is.na(CH4mean) & CH4mean != -999999) %>%
  group_by(Site_Nid) %>%
  mutate(n_means = n(),
         Month = month(Date_start)) %>%
  filter(n_means >= 20) %>%
  mutate(Month = factor(Month, 1:12), 
         Season_date = case_when(Month %in% c(1,2,12) ~ "Winter", 
                               Month %in% c(3:5) ~ "Spring", 
                               Month %in% c(6:8) ~ "Summer", 
                               Month %in% c(9:11) ~ "Autumn", 
         ), 
         Season_date = factor(Season_date, c("Winter", "Spring", 
                                             "Summer", "Autumn"))) %>%
  left_join(select(sites_df, Site_Nid, Latitude)) %>%
  mutate(Latitude = round(Latitude, 1), 
         Date2020 = as.Date(paste0("2020-", month(Date_start), "-", day(Date_start))))
  
site_arranged <- conc_ts %>%
  select(Site_Nid, Latitude)  %>%
  distinct() %>%
  arrange(desc(Latitude))

site_labels <- as.character(site_arranged$Latitude)
names(site_labels) <- as.character(site_arranged$Site_Nid)

conc_ts <- conc_ts %>%
  mutate(Site_Nid = factor(Site_Nid, site_arranged$Site_Nid))

season_colors <- c("blue4", "springgreen3", "Red", "goldenrod1")

CH4conc_ts_bysite <- ggplot(conc_ts, aes(x = Date_start, 
                                         y = CH4mean, 
                                         col = Season_date)) +
  geom_point() +
  scale_y_log10() + 
  facet_wrap(~Site_Nid, scales = "free", 
             labeller = labeller(Site_Nid = site_labels)) +
  theme_bw() +
  scale_color_manual(values = season_colors) + 
  theme(strip.background = element_blank(), 
        strip.text = element_text(size = 6), 
        axis.text = element_text(size = 6), 
        plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank()) +
  labs(y = expression(paste(CH[4], " (", mu, "M)"))) +
  ggtitle(expression(paste(CH[4], " concentrations at 20+ timepoints (title = latitude)"))) +
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.position = c(0.98, -0.03),
        legend.justification = c(1, 0), 
        legend.title.align = 0.5, 
        legend.box = "horizontal")

print(CH4conc_ts_bysite)

ggsave(file.path(path_to_dropbox, "Figures", "Timeseries", "CH4conc_20plus_sites_allyears.png"), 
       CH4conc_ts_bysite, 
       height = 10, width = 18)

CH4conc_ts_bysite_v2 <- ggplot(conc_ts, aes(x = Date2020, 
                                            y = CH4mean, 
                                            col = Season_date)) +
  geom_point() +
  scale_y_log10() + 
  facet_wrap(~Site_Nid, scales = "free_y", 
             labeller = labeller(Site_Nid = site_labels)) +
  theme_bw() +
  scale_color_manual(values = season_colors) + 
  theme(strip.background = element_blank(), 
        strip.text = element_text(size = 6), 
        axis.text = element_text(size = 6), 
        plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank()) +
  labs(y = expression(paste(CH[4], " (", mu, "M)"))) +
  ggtitle(expression(paste(CH[4], " concentrations at 20+ timepoints (title = latitude)"))) +
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.position = c(0.98, -0.03),
        legend.justification = c(1, 0), 
        legend.title.align = 0.5, 
        legend.box = "horizontal") +
  scale_x_date(date_labels = "%b-%d")

print(CH4conc_ts_bysite_v2)

ggsave(file.path(path_to_dropbox, "Figures", "Timeseries", "CH4conc_20plus_sites_annual.png"), 
       CH4conc_ts_bysite_v2, 
       height = 10, width = 18)


my_breaks = 10^seq(-6, 9)
CH4conc_ts_v3 <- ggplot(conc_ts, aes(x = Date2020, y = as.factor(Latitude), col = CH4mean)) +
  geom_point() +
  # geom_vpline() + 
  # scale_y_log10() + 
  # facet_wrap(~Site_Nid, scales = "free_y", 
             # labeller = labeller(Site_Nid = site_labels)) +
  theme_bw() +
  scale_color_distiller(palette = "YlOrBr", breaks = my_breaks, trans = "log", direction = 1) + 
  theme(axis.title.x = element_blank()) +
  labs(y = "Latitude")  + 
  # guides(color = guide_legend(nrow = 1)) +
  # theme(legend.position = c(0.95, -0.03),
  #       legend.justification = c(1, 0), 
  #       legend.title.align = 0.5, 
  #       legend.box = "horizontal") +
  # scale_x_date(date_labels = "%Y", date_breaks = "1 years") 
  scale_x_date(date_labels = "%b-%d")

print(CH4conc_ts_v3)

ggsave(file.path(path_to_dropbox, "Figures", "Timeseries", "CH4conc_20plus_sites_v3.png"), 
       CH4conc_ts_v3, 
       height = 8, width = 6)


##### By Q
CH4conc_Q_bysite <- conc_ts %>% 
  filter(Q> 0) %>% 
  ggplot( aes(x = Q, y = CH4mean, col = Season_date)) +
  geom_point() +
  scale_y_log10() + 
  facet_wrap(~Site_Nid, scales = "free", 
             labeller = labeller(Site_Nid = site_labels)) +
  theme_bw() +
  scale_color_manual(values = season_colors) + 
  theme(strip.background = element_blank(), 
        strip.text = element_text(size = 6), 
        axis.text = element_text(size = 6), 
        plot.title = element_text(hjust = 0.5)) +
  labs(y = expression(paste(CH[4], " (", mu, "M)")), 
       x = expression(paste("Discharge (m"^"3", " s"^"-1", ")"))) +
  ggtitle(expression(paste(CH[4], " concentrations at 20+ timepoints (title = latitude)"))) +
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.position = c(0.95, -0.03),
        legend.justification = c(1, 0), 
        legend.title.align = 0.5, 
        legend.box = "horizontal")

print(CH4conc_Q_bysite)

ggsave(file.path(path_to_dropbox, "Figures", "Timeseries", "CH4conc_Q.png"), 
       CH4conc_Q_bysite, 
       height = 10, width = 18)




conc_f <- conc_df %>%
  filter(!is.na(CH4mean)) %>%
  left_join(select(sites_df, Site_Nid, Latitude)) %>%
  mutate(Latitude_bin = case_when(Latitude >= 70 ~ "> 70",
                                  Latitude < 70 & Latitude >= 60 ~ "60 to 70", 
                                  Latitude < 60 & Latitude >= 50 ~ "50 to 60", 
                                  Latitude < 50 & Latitude >= 40 ~ "40 to 50", 
                                  Latitude < 40 & Latitude >= 30 ~ "30 to 40", 
                                  Latitude < 30 & Latitude >= 20 ~ "20 to 30", 
                                  Latitude < 20 & Latitude >= 10 ~ "10 to 20", 
                                  Latitude < 10 & Latitude >= 0 ~ "0 to 10", 
                                  Latitude < 0 & Latitude >= -10 ~ "-10 to 0", 
                                  Latitude < -10 & Latitude >= -20 ~ "-20 to -10", 
                                  Latitude < -20 & Latitude >= -30 ~ "-30 to -20", 
                                  Latitude < -30 & Latitude >= -40 ~ "-40 to -30", 
                                  Latitude < -40 & Latitude >= -50 ~ "-50 to -40", 
                                  Latitude < -50 ~ "< -50" )) %>%
  mutate(Latitude_bin = factor(Latitude_bin, c("< -50", "-50 to -40", 
                                               "-40 to -30", "-30 to -20",
                                               "-20 to -10", "-10 to 0",
                                               "0 to 10", "10 to 20",
                                               "20 to 30", "30 to 40",
                                               "40 to 50", "50 to 60",
                                               "60 to 70", "> 70")), 
         Month = month(Date_start),
         Date2020 = as.Date(paste0("2020-", month(Date_start), "-", day(Date_start)))) %>%
  group_by(Latitude_bin, Date2020) %>%
  summarize(n_obs = n())




flux_f <- flux_df %>%
  filter(!is.na(Diffusive_CH4_Flux_Mean)) %>%
  left_join(select(sites_df, Site_Nid, Latitude)) %>%
  mutate(Latitude_bin = case_when(Latitude >= 70 ~ "> 70",
                                  Latitude < 70 & Latitude >= 60 ~ "60 to 70", 
                                  Latitude < 60 & Latitude >= 50 ~ "50 to 60", 
                                  Latitude < 50 & Latitude >= 40 ~ "40 to 50", 
                                  Latitude < 40 & Latitude >= 30 ~ "30 to 40", 
                                  Latitude < 30 & Latitude >= 20 ~ "20 to 30", 
                                  Latitude < 20 & Latitude >= 10 ~ "10 to 20", 
                                  Latitude < 10 & Latitude >= 0 ~ "0 to 10", 
                                  Latitude < 0 & Latitude >= -10 ~ "-10 to 0", 
                                  Latitude < -10 & Latitude >= -20 ~ "-20 to -10", 
                                  Latitude < -20 & Latitude >= -30 ~ "-30 to -20", 
                                  Latitude < -30 & Latitude >= -40 ~ "-40 to -30", 
                                  Latitude < -40 & Latitude >= -50 ~ "-50 to -40", 
                                  Latitude < -50 ~ "< -50" )) %>%
  mutate(Latitude_bin = factor(Latitude_bin, c("< -50", "-50 to -40", 
                                               "-40 to -30", "-30 to -20",
                                               "-20 to -10", "-10 to 0",
                                               "0 to 10", "10 to 20",
                                               "20 to 30", "30 to 40",
                                               "40 to 50", "50 to 60",
                                               "60 to 70", "> 70")), 
         Month = month(Date_start),
         Date2020 = as.Date(paste0("2020-", month(Date_start), "-", day(Date_start)))) %>%
  group_by(Latitude_bin, Date2020) %>%
  summarize(n_obs = n())



conc_f_plot <- ggplot(filter(conc_f, !is.na(Latitude_bin))) +
  theme_bw() + 
  geom_point(aes(y=Latitude_bin, x = Date2020, size = n_obs), shape = 3, col = "darkblue", stroke = .55, alpha = 1) + 
  scale_x_date(date_labels = "%b-%d", date_breaks = "2 months", expand = c(0.01, 0.01)) +
  scale_y_discrete(drop = FALSE) + 
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom", 
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 8)) + 
  labs(y = "Latitude bin (deg)", size = "Number of concentration obs") +
  scale_size_continuous(breaks = c(1, 10, 100, 200), range = c(1, 10), trans = "sqrt")

print(conc_f_plot)

ggsave(file.path(path_to_dropbox, "Figures", "Timeseries", "CH4conc_frequency_by_latitude.png"), 
       flux_f_plot, 
       height = 4.5, width = 4)


flux_f_plot <- ggplot(filter(flux_f, !is.na(Latitude_bin))) +
  theme_bw() + 
  geom_point(aes(y=Latitude_bin, x = Date2020, size = n_obs), shape = 3, col = "darkblue", stroke = .55, alpha = 1) + 
  scale_x_date(date_labels = "%b-%d", date_breaks = "2 months", expand = c(0.01, 0.01)) +
  scale_y_discrete(drop = FALSE) + 
  theme(axis.title.x = element_blank(), 
        legend.position = "bottom", 
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 8)) + 
  labs(y = "Latitude bin (deg)", size = "Number of diffusive flux obs") +
  scale_size_continuous(breaks = c(1, 10, 100, 200), range = c(1, 10), trans = "sqrt")

print(flux_f_plot)

ggsave(file.path(path_to_dropbox, "Figures", "Timeseries", "CH4flux_frequency_by_latitude.png"), 
       flux_f_plot, 
       height = 4.5, width = 4)

flux_f_month <- flux_f %>% 
  mutate(month = month(Date2020)) %>%
  group_by(Latitude_bin, month) %>%
  summarize(n_obs = sum(n_obs)) %>%
  mutate(month_abb = factor(month.abb[as.numeric(month)], month.abb[1:12]), 
         Latitude_bin_flip = factor(Latitude_bin, rev(levels(Latitude_bin))), 
         type = factor("Flux", levels = c("Concentration", "Both", "Flux")))

flux_f_plot2 <- ggplot(filter(flux_f_month, !is.na(Latitude_bin))) +
  theme_bw() + 
  theme_grime() + 
  geom_col(aes(y = n_obs, x = month_abb, fill = type), 
           # fill = "steelblue3", 
           color = "grey10") +
  facet_grid(rows = vars(Latitude_bin_flip), drop = FALSE) + 
  scale_y_sqrt(expand = expansion(mult = c(0,.1))) +
  labs(y = expression(paste("Number of ", CH[4], " flux observations")),
       x = "Month", 
       title = expression(paste("Number of flux observations by month and latitude (", degree, ")"))) +
  theme(strip.text.y = element_text(angle = 0), 
        strip.background = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 10), 
        axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 5), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(legend.position = "none")

print(flux_f_plot2)

ggsave(file.path(path_to_dropbox, "Figures", "Timeseries",
                 "CH4flux_frequency_by_latitude_month.png"), 
       flux_f_plot2, 
       height = 7.5, width = 4)



conc_f_month <- conc_f %>% 
  mutate(month = month(Date2020)) %>%
  group_by(Latitude_bin, month) %>%
  summarize(n_obs = sum(n_obs)) %>%
  mutate(month_abb = factor(month.abb[as.numeric(month)], month.abb[1:12]), 
         Latitude_bin_flip = factor(Latitude_bin, rev(levels(Latitude_bin))), 
         type = factor("Concentration", levels = c("Concentration", "Both", "Flux")))

conc_f_plot2 <- ggplot(filter(conc_f_month, !is.na(Latitude_bin))) +
  theme_bw() + 
  theme_grime() + 
  geom_col(aes(y = n_obs, x = month_abb, fill = type), 
           # fill = "darkgoldenrod2",
           color = "grey10") +
  facet_grid(rows = vars(Latitude_bin_flip), drop = FALSE) + 
  scale_y_sqrt(expand = expansion(mult = c(0,.1))) +
  labs(y = expression(paste("Number of ", CH[4], " concentration observations")),
       x = "Month", 
       facet = "Latitude",
       title = expression(paste("Number of concentration observations by month and latitude (", degree, ")"))) +
  theme(strip.text.y = element_text(angle = 0), 
        strip.background = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 10), 
        axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 5), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank()) +
  theme(legend.position = "none")
  

print(conc_f_plot2)

ggsave(file.path(path_to_dropbox, "Figures", "Timeseries",
                 "CH4conc_frequency_by_latitude_month.png"), 
       conc_f_plot2, 
       height = 7.5, width = 4)



both_f_month <- flux_f_month %>%
  full_join((conc_f_month))



both_f_plot2 <- ggplot(filter(both_f_month, !is.na(Latitude_bin))) +
  theme_bw() + 
  theme_grime() + 
  geom_col(aes(y = as.numeric(n_obs), x = month_abb, fill = type), 
           color = "grey10") +
  # scale_fill_manual(values = c("darkgoldenrod2", "steelblue3")) + 
  scale_y_sqrt(expand = expansion(mult = c(0,.1))) +
  facet_grid(rows = vars(Latitude_bin_flip), 
             cols = vars(type),
             drop = TRUE) + 
  labs(y = expression(paste("Number of observations")),
       x = "Month", 
       title = expression(paste("Number of observations by month and latitude (", degree, ")"))) +
  theme(strip.text.y = element_text(angle = 0), 
        strip.background = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 10), 
        axis.text.x = element_text(size = 7), 
        axis.text.y = element_text(size = 5), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = "none")

print(both_f_plot2)

ggsave(file.path(path_to_dropbox, "Figures", "Timeseries",
                 "CH4concflux_frequency_by_latitude_month.png"), 
       both_f_plot2, 
       height = 7.5, width = 6)


