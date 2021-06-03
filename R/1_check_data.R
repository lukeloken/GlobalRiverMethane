

load(file.path(path_to_dropbox, "db_processingR", 
               "MethDB_tables_converted.rda"))


summary(conc_df$CH4mean)


ggplot(conc_df, aes(x = orig_CH4unit, y = CH4mean)) + 
  geom_jitter(alpha = 0.2, width = 0.3, height = 0, color = "red") + 
  geom_boxplot(outlier.shape = NA, fill = NA, color = "darkblue", lwd = 1.5) + 
  scale_y_log10(breaks = 10^ seq(-10, 10, 1)) +
  theme_bw()

#N2O
ggplot(conc_df, aes(x = orig_N2Ounit, y = N2Omean)) + 
  geom_jitter(alpha = 0.2, width = 0.3, height = 0, color = "red") + 
  geom_boxplot(outlier.shape = NA, fill = NA, color = "darkblue", lwd = 1.5) + 
  scale_y_log10(breaks = 10^ seq(-10, 10, 1)) +
  theme_bw()

highN2O <- filter(conc_df, 
                  orig_N2Ounit %in% c("mgN/L", "nmol/L", "umol/L"), 
                  N2Omean > 5)

lowN2O <- filter(conc_df, 
                 orig_N2Ounit %in% c("mgN/L", "nmol/L", "umol/L"), 
                 N2Omean < .001)

badN2O <- bind_rows(highN2O, lowN2O)

unique(lowN2O$Publication_Nid)
unique(highN2O$Publication_Nid)

N2O_df <- conc_df %>%
  mutate(Publication_Nid = factor(Publication_Nid, 
                                  c(unique(badN2O$Publication_Nid), "other"))) %>%
  mutate(Publication_Nid = ifelse(is.na(Publication_Nid), 
                                  "other", 
                                  as.character(Publication_Nid)))


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

ggplot(flux_df, aes(x = DiffusiveFluxunit, y = DiffusiveCH4FluxMean)) + 
  geom_jitter(alpha = 0.2, width = 0.3, height = 0, color = "red") + 
  geom_boxplot(outlier.shape = NA, fill = NA, color = "darkblue", lwd = 1.5) + 
  scale_y_log10(breaks = 10^ seq(-10, 10, 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))


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
  geom_histogram(aes(x = DiffusiveCH4FluxMean)) +
  scale_x_log10()

ggplot(flux_df) +
  geom_histogram(aes(x = BubbleCH4FluxMean)) +
  scale_x_log10()

ggplot(flux_df) +
  geom_histogram(aes(x = TotalCH4FluxMean)) +
  scale_x_log10()

ggplot(flux_df) +
  geom_histogram(aes(x = CO2FluxMean)) +
  scale_x_log10()

ggplot(flux_df) +
  geom_histogram(aes(x = N2OFluxMean)) +
  scale_x_log10()
