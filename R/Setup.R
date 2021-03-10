# Setup file

# local path to your dropbox folder. 
# path_to_dropbox <- 'C:/Dropbox/MethDb'

#Replace with your own dropbox folder location
path_to_dropbox <- "C:/Users/emsta/Dropbox/MethDB2.0" #Emily's folder
path_to_dropbox <- "C:/Users/lloken/OneDrive - DOI/GlobalRiverMethane" #Luke's USGS computer

# MethDB_filename <- "MethDB_EHS_18Jan21.xlsx"
# MethDB_filename <- "MethDB_EHS_25Jan21.xlsx"
# MethDB_filename <- "MethDB_EHS_5Feb21.xlsx"
# MethDB_filename <- "MethDB_1Mar2020.xlsx"
MethDB_filename <- "MethDB_1Mar2021.xlsx"



#Non set up below

list.files(path_to_dropbox)


# load formatted and converted tables into your R environment
load(file.path(path_to_dropbox, "db_processingR", 
               "MethDB_tables_converted.rda"))
