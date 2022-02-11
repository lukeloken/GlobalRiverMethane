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
MethDB_filename <- "MethDB_27Nov2021.xlsx"



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

theme_grime <- function(){
  list(
    scale_color_manual(values = c("darkgoldenrod2",  "steelblue3", "olivedrab3")), 
    scale_fill_manual(values = c("darkgoldenrod2", "steelblue3", "olivedrab3"))
  ) 
}
