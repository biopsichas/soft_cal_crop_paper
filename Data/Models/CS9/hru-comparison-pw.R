# Required library
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(beepr)

setwd("D:/SWAT/FamR/FarmR_2002_2020/colture_splittate_SQ/setup_20240414_diversion")
# Set the code of the hru
hru_name1 = 'hru0012' # hru735=frst/spa1, hru743=frst/gvn1, hru331 or hru333=frst/spa1, hru891=past/VLS1
hru_name2 = 'hru0025' # hru735=frst/spa1, hru743=frst/gvn1, hru331 or hru333=frst/spa1, hru891=past/VLS1

# Set the type of plot to make
analysis_type = 'prod'#'cn' #'water_balance' # 'evapotranspiration' #snow #water_balance #soil_balance #lat_flow


# Set the SWAT+ output file
filename = 'hru_pw_day.txt'


colNames = c("jday", "mon", "day", "yr", "unit", "gis_id", 
             "name", "lai", "bioms", "yield", "residue", "sol_tmp", 
             "strsw", "strsa", "strstmp", "strsn", "strsp", "strss", 
             "nplt", "percn", "pplnt", "tmx", "tmn", "tmpav", "solarad", "wndspd", 
             "rhum", "phubas0", "lai_max", "bm_max", "bm_grow", "c_gro")

# legge il file come tabella
df = read.delim(filename, header = FALSE, sep = "", dec = ".", skip =3, col.names= colNames)

beep()

# seleziona solo le righe che corrispondono a hru_name
sub_df1 = subset(df,df$name==hru_name1)
sub_df1 = subset(sub_df1,sub_df1$yr>=1994)
#
sub_df2 = subset(df,df$name==hru_name2)
sub_df2 = subset(sub_df2,sub_df2$yr>=1994)

# crea un campo con le date
sub_ts= ISOdatetime(sub_df1$yr,sub_df1$mon,sub_df1$day,12,0,0)


if (analysis_type == 'prod'){
  # crea una tabella con i campi di interesse
  alldata = data.frame(date = sub_ts,
                       bioms1=sub_df1$bioms,
                       bioms2=sub_df2$bioms)
  
  # crea un plot
  p <- ggplot(alldata, aes(date)) +
    geom_line(aes(y = bioms1, colour  = "frst")) +
    geom_line(aes(y = bioms2, colour  = "orcd")) +
    ylab("t/ha)") +
    ggtitle(hru_name1) +
    theme_ipsum()
}

# rende il plot interattivo
p <- ggplotly(p)
p
