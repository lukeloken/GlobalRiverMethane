

load(file.path(path_to_dropbox, "db_processingR", 
               "MethDB_tables_converted.rda"))


summary(conc_df$CH4mean)


#Quick plots of distributions
ggplot(conc_df) +
  geom_histogram(aes(x = CH4mean)) +
  scale_x_log10() +
  geom_vline(xintercept = 0.003)


undersaturdated <- filter(conc_df, CH4mean < 0.003) %>%
  arrange(Publication_Nid, desc(CH4mean)) %>%
  select(c("Publication_Nid", "Site_Nid", "SiteName", "CH4mean", "orig_CH4unit", "new_CH4unit"))

table(undersaturdated$Publication_Nid)

CH4mean_2315 <- ggplot(filter(conc_df, Publication_Nid == "2315")) +
  geom_hline(yintercept = 0.003) + 
  geom_point(aes(x = SampleDatestart, y = CH4mean)) +
  scale_y_log10() +
  facet_wrap(~SiteName)  +
  ggtitle("Publication 2315")

CH4mean_2309 <- ggplot(filter(conc_df, Publication_Nid == "2309")) +
  geom_hline(yintercept = 0.003) + 
  geom_point(aes(x = SampleDatestart, y = CH4mean)) +
  scale_y_log10() +
  facet_wrap(~SiteName) +
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
