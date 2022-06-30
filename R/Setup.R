# Setup file

# local path to your dropbox folder. 
# path_to_dropbox <- 'C:/Dropbox/MethDb'

#Replace with your own dropbox folder location
# path_to_dropbox <- "C:/Users/emsta/Dropbox/MethDB2.0" #Emily's folder
path_to_dropbox <- "C:/Users/lloken/OneDrive - DOI/GlobalRiverMethane" #Luke's USGS computer

# path_to_dropbox <-  "C:/Users/gero0008/Dropbox/SCIENCE/PostDoc/MethDB2.0" #gerards pc

# MethDB_filename <- "MethDB_EHS_18Jan21.xlsx"
# MethDB_filename <- "MethDB_EHS_25Jan21.xlsx"
# MethDB_filename <- "MethDB_EHS_5Feb21.xlsx"
# MethDB_filename <- "MethDB_1Mar2020.xlsx"
# MethDB_filename <- "MethDB_20Mar2021.xlsx"
# MethDB_filename <- "MethDB_5Apr2021.xlsx"
# MethDB_filename <- "MethDB_1Aug2021.xlsx"
# MethDB_filename <- "MethDB_2Aug2021.xlsx"
# MethDB_filename <- "MethDB_12Aug2021.xlsx"
# MethDB_filename <- "MethDB_16Aug2021.xlsx"
# MethDB_filename <- "MethDB_28Sep2021.xlsx"
# MethDB_filename <- "MethDB_27Nov2021.xlsx"
# MethDB_filename <- "MethDB_18Feb2022.xlsx"
# MethDB_filename <- "MethDB_9Apr2022.xlsx"
# MethDB_filename <- "MethDB_20Jun2022.xlsx"
MethDB_filename <- "MethDB_23Jun2022.xlsx"


#Non set up below

list.files(path_to_dropbox)

if (MethDB_filename %in% list.files(path_to_dropbox)){
  print("File name exists in folder")
} else {
  warning("MethDB file not in directory. Check name and path")
}


# load formatted and converted tables into your R environment
load(file.path(path_to_dropbox, "db_processingR", 
               "MethDB_tables_converted.rda"))

# Find newest GRIME table and load into your R environment
db_processing_files <- list.files(file.path(path_to_dropbox, "db_processingR"))
grime_tables <- db_processing_files[grepl("GRiMe_tables_converted_Internal", db_processing_files)]
grime_table_dates <- gsub("GRiMe_tables_converted_Internal_", "", grime_tables)
grime_table_dates <- gsub(".rda", "", grime_table_dates) %>% as.Date()
newest_file_number <- which(grime_table_dates == max(grime_table_dates, na.rm = TRUE))
newest_file_name <- grime_tables[newest_file_number]
load(file.path(path_to_dropbox, 
               "db_processingR", 
               newest_file_name))

theme_grime <- function(){
  list(
    scale_color_manual(values = c("#ff9f1c",  "#936639", "#a4ac86"), drop = FALSE), 
    scale_fill_manual(values = c("#ff9f1c", "#936639", "#a4ac86"), drop = FALSE)
  ) 
}

#Test code for theme grime
library(egg)
library(ggplot2)

x = runif(100, 0, 40)
a = sample(0:2, 100, replace = TRUE)
type = as.factor(a)
levels(type) <- c("conc", "both", "flux")
y =  a*20 + x + rnorm(100, 0, 15)
df_test <- data.frame(x, a, type, y)

p2 <- ggplot(df_test) +
  geom_boxplot(aes(x = type, y = y, fill = type, group = type)) +
  theme_bw() + 
  theme_grime()

p1 <- ggplot(df_test, aes(x = x, y = y)) +
  geom_point(aes(color = type), alpha = 0.3) +
  geom_smooth(aes(color = type), method = "lm", se = FALSE, size = 2) + 
  theme_bw() +
  theme_grime()

p3 <- p1 + theme_dark()
p4 <- p2 + theme_dark()
p5 <- p1 + geom_point(aes(color = type), alpha = 1) 
p6 <- p3 + geom_point(aes(color = type), alpha = 1) 

egg::ggarrange(plots = list(p1, p5, p2, p3, p6, p4), byrow = FALSE)

