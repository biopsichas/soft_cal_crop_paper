# Required library
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)

setwd("D:/articolo_chandawa/setup_20240414_diversion")
# Set the code of the hru
hru_name = 'hru0012' # hru735=frst/spa1, hru743=frst/gvn1, hru331 or hru333=frst/spa1, hru891=past/VLS1

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

install.packages("beepr")
library(beepr)
beep()

# seleziona solo le righe che corrispondono a hru_name
sub_df = subset(df,df$name==hru_name)
sub_df = subset(sub_df,sub_df$yr>=1994)

# crea un campo con le date
sub_ts= ISOdatetime(sub_df$yr,sub_df$mon,sub_df$day,12,0,0)

if (analysis_type == 'stress'){
  # crea una tabella con i campi di interesse
  alldata = data.frame(date = sub_ts,
                       strsw=sub_df$strsw,
                       strsa=sub_df$strsa,
                       strstmp=sub_df$strstmp,
                       strsn = sub_df$strsn,
                       strsp = sub_df$strsp)
  # crea un plot
  p <- ggplot(alldata, aes(date)) +
    geom_line(aes(y = strsw, colour  = "strsw")) +
    geom_line(aes(y = strsa, colour  = "strsa")) +
    geom_line(aes(y = strstmp, colour  = "strstmp")) +
    geom_line(aes(y = strsn, colour  = "strsn")) +
    geom_line(aes(y = strsp, colour  = "strsp")) +
    ylab("stress (cioccolatini)") +
    ggtitle(hru_name) +
    theme_ipsum()
}

# rende il plot interattivo
p <- ggplotly(p)
p

if (analysis_type == 'prod'){
  # crea una tabella con i campi di interesse
  alldata = data.frame(date = sub_ts,
                       bioms=sub_df$bioms,
                       yield=sub_df$yield)
                       
  # crea un plot
  p <- ggplot(alldata, aes(date)) +
    geom_line(aes(y = bioms, colour  = "bioms")) +
    geom_line(aes(y = yield, colour  = "yield")) +
    ylab("nikkonikkoni (cioccolatini)") +
    ggtitle(hru_name) +
    theme_ipsum()
}

# rende il plot interattivo
p <- ggplotly(p)
p
