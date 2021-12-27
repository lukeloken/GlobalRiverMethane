
library(RColorBrewer)
library(dplyr)
library(ggplot2)

load(file.path(path_to_dropbox, "db_processingR", 
               "MethDB_tables_converted.rda"))


summary(conc_df$CH4mean)


ggplot(conc_df, aes(x = orig_CH4unit, y = CH4mean)) + 
  geom_jitter(alpha = 0.2, width = 0.3, height = 0, color = "red") + 
  geom_boxplot(outlier.shape = NA, fill = NA, color = "darkblue", lwd = 1.5) + 
  scale_y_log10(breaks = 10^ seq(-10, 10, 1)) +
  theme_bw()

highCH4 <- filter(conc_df, !is.na(CH4mean), CH4mean > 0, CH4mean != -999999) %>%
  filter(# orig_CH4unit %in% c("mol/L")
         CH4mean > 10000 |
         CH4mean < 0.0001)

unique(highCH4$Publication_Nid)

CH4_df <- conc_df %>%
  mutate(Publication_Nid = factor(Publication_Nid, 
                                  c(unique(highCH4$Publication_Nid), "other"))) %>%
  mutate(Publication_Nid = ifelse(is.na(Publication_Nid), 
                                  "other", 
                                  as.character(Publication_Nid))) %>%
  arrange(desc(Publication_Nid))

ggplot(CH4_df, aes(x = orig_CH4unit, y = CH4mean)) + 
  geom_jitter(alpha = 0.5, width = 0.3, height = 0, aes(color = Publication_Nid)) +
  # geom_boxplot(outlier.shape = NA, fill = NA, color = "darkblue", lwd = 1) + 
  scale_y_log10(breaks = 10^ seq(-10, 10, 1)) +
  theme_bw() + 
  scale_color_manual(values = c(brewer.pal(length(unique(CH4_df$Publication_Nid)) - 1, 
                                           "Dark2"), "lightgrey")) 

#N2O
ggplot(conc_df, aes(x = orig_N2Ounit, y = N2Omean)) + 
  geom_jitter(alpha = 0.2, width = 0.3, height = 0, color = "red") + 
  geom_boxplot(outlier.shape = NA, fill = NA, color = "darkblue", lwd = 1.5) + 
  scale_y_log10(breaks = 10^ seq(-10, 10, 1)) +
  theme_bw()

highN2O <- filter(conc_df, N2Omean != -999999,
                  # orig_N2Ounit %in% c("mgN/L", "nmol/L", "umol/L"), 
                  N2Omean > 5)

lowN2O <- filter(conc_df, N2Omean != -999999,
                 # orig_N2Ounit %in% c("mgN/L", "nmol/L", "umol/L"), 
                 N2Omean < .001)

badN2O <- bind_rows(highN2O, lowN2O)

unique(lowN2O$Publication_Nid)
unique(highN2O$Publication_Nid)

N2O_df <- conc_df %>%
  mutate(Publication_Nid = factor(Publication_Nid, 
                                  c(unique(badN2O$Publication_Nid), "other"))) %>%
  mutate(Publication_Nid = ifelse(is.na(Publication_Nid), 
                                  "other", 
                                  as.character(Publication_Nid))) %>%
  arrange(desc(Publication_Nid))


ggplot(N2O_df, aes(x = orig_N2Ounit, y = N2Omean)) + 
  geom_jitter(alpha = 0.5, width = 0.3, height = 0, aes(color = Publication_Nid)) +
  geom_boxplot(outlier.shape = NA, fill = NA, color = "darkblue", lwd = 1) + 
  scale_y_log10(breaks = 10^ seq(-10, 10, 1)) +
  theme_bw() + 
  scale_color_manual(values = c(brewer.pal(length(unique(N2O_df$Publication_Nid)) - 1, 
                                           "Paired"), "lightgrey")) #+
# geom_jitter(alpha = 0.5, width = 0.3, height = 0, 
#             data = badN2O, aes(col = as.factor(Publication_Nid)))




#CO2
ggplot(conc_df, aes(x = orig_CO2unit, y = CO2mean)) + 
  geom_jitter(alpha = 0.2, width = 0.3, height = 0, color = "red") + 
  geom_boxplot(outlier.shape = NA, fill = NA, color = "darkblue", lwd = 1.5) + 
  scale_y_log10(breaks = 10^ seq(-10, 10, 1)) +
  theme_bw()


highCO2 <- filter(conc_df, CO2mean != -999999,
                  CO2mean > 10000 |
                    CO2mean < 0.1)

unique(highCO2$Publication_Nid)

CO2_df <- conc_df %>%
  mutate(Publication_Nid = factor(Publication_Nid, 
                                  c(unique(highCO2$Publication_Nid), "other"))) %>%
  mutate(Publication_Nid = ifelse(is.na(Publication_Nid), 
                                  "other", 
                                  as.character(Publication_Nid))) %>%
  arrange(desc(Publication_Nid))

ggplot(CO2_df, aes(x = orig_CO2unit, y = CO2mean)) + 
  geom_jitter(alpha = 0.5, width = 0.3, height = 0, aes(color = Publication_Nid)) +
  # geom_boxplot(outlier.shape = NA, fill = NA, color = "darkblue", lwd = 1) + 
  scale_y_log10(breaks = 10^ seq(-10, 10, 1)) +
  theme_bw() + 
  scale_color_manual(values = c(brewer.pal(length(unique(CO2_df$Publication_Nid)) - 1, 
                                           "Paired"), "lightgrey")) 

#CH4
ggplot(conc_df, aes(x = orig_CH4unit, y = CH4mean)) + 
  geom_jitter(alpha = 0.2, width = 0.3, height = 0, color = "red") + 
  geom_boxplot(outlier.shape = NA, fill = NA, color = "darkblue", lwd = 1.5) + 
  scale_y_log10(breaks = 10^ seq(-10, 10, 1)) +
  theme_bw()

#Look at ppm CH4 papers
ggplot(filter(conc_df, orig_CH4unit == "ppm CH4"), aes(x = as.factor(Publication_Nid),
                                                       y = CH4mean)) + 
  geom_jitter(alpha = 0.2, width = 0.3, height = 0, color = "red") + 
  geom_boxplot(outlier.shape = NA, fill = NA, color = "darkblue", lwd = 1.5) + 
  scale_y_log10(breaks = 10^ seq(-10, 10, 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45)) +
  ggtitle("original CH4 unit = ppm CH4")


#CH4 flux
ggplot(flux_df, aes(x = Diffusive_Flux_unit, y = Diffusive_CH4_Flux_Mean)) + 
  geom_jitter(alpha = 0.2, width = 0.3, height = 0, color = "red") + 
  geom_boxplot(outlier.shape = NA, fill = NA, color = "darkblue", lwd = 1.5) + 
  scale_y_log10(breaks = 10^ seq(-10, 10, 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))


highCH4 <- filter(flux_df, !is.na(Diffusive_CH4_Flux_Mean), 
                  Diffusive_CH4_Flux_Mean > 0, Diffusive_CH4_Flux_Mean != -999999) %>%
  filter(# orig_CH4unit %in% c("mol/L")
    Diffusive_CH4_Flux_Mean > 1000)

unique(highCH4$Publication_Nid)

CH4_flux_df <- flux_df %>%
  mutate(Publication_Nid = factor(Publication_Nid, 
                                  c(unique(highCH4$Publication_Nid), "other"))) %>%
  mutate(Publication_Nid = ifelse(is.na(Publication_Nid), 
                                  "other", 
                                  as.character(Publication_Nid))) %>%
  arrange(desc(Publication_Nid))

ggplot(CH4_flux_df, aes(x = Diffusive_Flux_unit, y = Diffusive_CH4_Flux_Mean)) + 
  geom_jitter(alpha = 0.5, width = 0.3, height = 0, aes(color = Publication_Nid)) +
  # geom_boxplot(outlier.shape = NA, fill = NA, color = "darkblue", lwd = 1) + 
  scale_y_log10(breaks = 10^ seq(-10, 10, 1)) +
  theme_bw() + 
  scale_color_manual(values = c(brewer.pal(length(unique(CH4_flux_df$Publication_Nid)) - 1, 
                                           "Dark2"), "lightgrey")) +
  coord_flip()





#CO2 flux
ggplot(flux_df, aes(x = CO2_Flux_unit, y = CO2_Flux_Mean)) + 
  geom_jitter(alpha = 0.2, width = 0.3, height = 0, color = "red") + 
  geom_boxplot(outlier.shape = NA, fill = NA, color = "darkblue", lwd = 1.5) + 
  # scale_y_log10(breaks = 10^ seq(-10, 10, 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))


highCO2 <- filter(flux_df, !is.na(CO2_Flux_Mean), CO2_Flux_Mean != -999999) %>%
  filter(# orig_CH4unit %in% c("mol/L")
    CO2_Flux_Mean < -50 | CO2_Flux_Mean > 5000)

unique(highCO2$Publication_Nid)

CO2_flux_df <- flux_df %>%
  mutate(Publication_Nid = factor(Publication_Nid, 
                                  c(unique(highCO2$Publication_Nid), "other"))) %>%
  mutate(Publication_Nid = ifelse(is.na(Publication_Nid), 
                                  "other", 
                                  as.character(Publication_Nid))) %>%
  arrange(desc(Publication_Nid))

ggplot(CO2_flux_df, aes(x = CO2_Flux_unit, y = CO2_Flux_Mean)) + 
  geom_jitter(alpha = 0.5, width = 0.3, height = 0, aes(color = Publication_Nid)) +
  # geom_boxplot(outlier.shape = NA, fill = NA, color = "darkblue", lwd = 1) + 
  # scale_y_log10(breaks = 10^ seq(-10, 10, 1)) +
  theme_bw() + 
  # scale_color_manual(values = c(brewer.pal(length(unique(CO2_flux_df$Publication_Nid)) - 1, 
                                           # "Paired"), "lightgrey")) +
  coord_flip()

#N2O flux
ggplot(flux_df, aes(x = N2O_Flux_unit, y = N2O_Flux_Mean)) + 
  geom_jitter(alpha = 0.2, width = 0.3, height = 0, color = "red") + 
  geom_boxplot(outlier.shape = NA, fill = NA, color = "darkblue", lwd = 1.5) + 
  scale_y_log10(breaks = 10^ seq(-10, 10, 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

highN2O <- filter(flux_df, !is.na(N2O_Flux_Mean), N2O_Flux_Mean > 0, N2O_Flux_Mean != -999999) %>%
  filter(# orig_CH4unit %in% c("mol/L")
    N2O_Flux_Mean > 1)

unique(highN2O$Publication_Nid)

N2O_flux_df <- flux_df %>%
  mutate(Publication_Nid = factor(Publication_Nid, 
                                  c(unique(highN2O$Publication_Nid), "other"))) %>%
  mutate(Publication_Nid = ifelse(is.na(Publication_Nid), 
                                  "other", 
                                  as.character(Publication_Nid))) %>%
  arrange(desc(Publication_Nid))

ggplot(N2O_flux_df, aes(x = N2O_Flux_unit, y = N2O_Flux_Mean)) + 
  geom_jitter(alpha = 0.5, width = 0.3, height = 0, aes(color = Publication_Nid)) +
  # geom_boxplot(outlier.shape = NA, fill = NA, color = "darkblue", lwd = 1) + 
  scale_y_log10(breaks = 10^ seq(-10, 10, 1)) +
  theme_bw() + 
  scale_color_manual(values = c(brewer.pal(length(unique(N2O_flux_df$Publication_Nid)) - 1, 
                                           "Dark2"), "lightgrey")) +
  coord_flip()








ggplot(conc_df, aes(x = CH4mean)) + 
  facet_wrap(~orig_CH4unit, scales = "free_y") + 
  geom_histogram() + 
  # geom_jitter(alpha = 0.2, width = 0.3, height = 0, color = "red") + 
  # geom_boxplot(outlier.shape = NA, fill = NA, color = "darkblue", lwd = 1.5) + 
  scale_x_log10(breaks = 10^ seq(-10, 10, 1)) +
  theme_bw()




#Quick plots of distributions
ggplot(conc_df) +
  geom_histogram(aes(x = CH4mean)) +
  scale_x_log10() +
  geom_vline(xintercept = 0.003)


undersaturdated <- filter(conc_df, CH4mean < 0.003) %>%
  arrange(Publication_Nid, desc(CH4mean)) %>%
  select(c("Publication_Nid", "Site_Nid", "Site_Name", "CH4mean", "orig_CH4unit", "new_CH4unit"))

table(undersaturdated$Publication_Nid)

CH4mean_2315 <- ggplot(filter(conc_df, Publication_Nid == "2315")) +
  geom_hline(yintercept = 0.003) + 
  geom_point(aes(x = Date_start, y = CH4mean)) +
  scale_y_log10() +
  facet_wrap(~Site_Name)  +
  ggtitle("Publication 2315")

CH4mean_2309 <- ggplot(filter(conc_df, Publication_Nid == "2309")) +
  geom_hline(yintercept = 0.003) + 
  geom_point(aes(x = Date_start, y = CH4mean)) +
  scale_y_log10() +
  facet_wrap(~Site_Name) +
  ggtitle("Publication 2309")


ggsave(file.path(path_to_dropbox, "Figures", "Timeseries", "CH4conc_Pub2309.png"), 
       CH4mean_2309, 
       height = 10, width = 12)

ggsave(file.path(path_to_dropbox, "Figures", "Timeseries", "CH4conc_Pub2315.png"), 
       CH4mean_2315, 
       height = 10, width = 12)


ggplot(conc_df) +
  geom_histogram(aes(x = CO2mean)) +
  scale_x_log10()
  

ggplot(conc_df) +
  geom_histogram(aes(x = N2Omean)) +
  scale_x_log10()

ggplot(conc_df) +
  geom_histogram(aes(x = CH4min)) +
  scale_x_log10()

ggplot(conc_df) +
  geom_histogram(aes(x = CH4max)) +
  scale_x_log10()

ggplot(conc_df) +
  geom_histogram(aes(x = CH4median)) +
  scale_x_log10()

ggplot(flux_df) +
  geom_histogram(aes(x = Diffusive_CH4_Flux_Mean)) +
  scale_x_log10()

ggplot(flux_df) +
  geom_histogram(aes(x = Eb_CH4_Flux_Mean)) +
  scale_x_log10()

ggplot(flux_df) +
  geom_histogram(aes(x = Total_CH4_Flux_Mean)) +
  scale_x_log10()

ggplot(flux_df) +
  geom_histogram(aes(x = CO2_Flux_Mean)) +
  scale_x_log10()

ggplot(flux_df) +
  geom_histogram(aes(x = N2O_Flux_Mean)) +
  scale_x_log10()

