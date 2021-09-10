

library(tidyverse)
library(ggplot2)
library(lubridate)

load(file.path(path_to_dropbox, "db_processingR", 
               "MethDB_tables_converted.rda"))


conc_ts <- conc_df %>%
  filter(!is.na(CH4mean)) %>%
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
  theme(legend.position = c(0.95, -0.03),
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
  theme(legend.position = c(0.95, -0.03),
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
  mutate(Latitude_bin = case_when(Latitude >= 60 ~ "60+",
                                  Latitude < 60 & Latitude >= 45 ~ "45 to 60", 
                                  Latitude < 45 & Latitude >= 30 ~ "30 to 45", 
                                  Latitude < 30 & Latitude >= 15 ~ "15 to 30", 
                                  Latitude < 15 & Latitude >= 0 ~ "0 to 15", 
                                  Latitude < 0 & Latitude >= -15 ~ "-15 to 0", 
                                  Latitude < -15 & Latitude >= -30 ~ "-30 to -15", 
                                  Latitude < -30 & Latitude >= -45 ~ "-45 to -30", 
                                  Latitude < -45 & Latitude >= -60 ~ "-60 to -45", 
                                  Latitude < -60 & Latitude >= 45 ~ "-60-" )) %>%
  mutate(Latitude_bin = factor(Latitude_bin, c("-60-", "-60 to -45", 
                                               "-45 to -30", "-30 to -15",
                                               "-15 to 0", "0 to 15",
                                               "15 to 30", "30 to 45",
                                               "45 to 60", "60+")), 
         Month = month(SampleDatestart),
         Date2020 = as.Date(paste0("2020-", month(SampleDatestart), "-", day(SampleDatestart)))) %>%
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
  labs(y = "Latitude bin (deg)", size = "Number of obs") +
  scale_size_continuous(breaks = c(1, 10, 100, 200), range = c(1, 10), trans = "sqrt")

print(conc_f_plot)

ggsave(file.path(path_to_dropbox, "Figures", "Timeseries", "CH4conc_frequency_by_latitude.png"), 
       conc_f_plot, 
       height = 4.5, width = 4)


