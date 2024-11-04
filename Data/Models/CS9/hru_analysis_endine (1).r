# Very important note: daily hru water balance outputs must be selected before simulation (hru_wb_day.txt)

# Required library
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)


# Set the code of the hru
hru_name = 'hru0007' # hru735=frst/spa1, hru743=frst/gvn1, hru331 or hru333=frst/spa1, hru891=past/VLS1

# Set the type of plot to make
analysis_type = 'water_balance'#'cn' #'water_balance' # 'evapotranspiration' #snow #water_balance #soil_balance #lat_flow


# Set the SWAT+ output file
filename = 'hru_wb_day.txt'


colNames = c('jday','mon','day','yr','unit','gis_id','name','precip','snofall','snomlt',
             'surq_gen','latq','wateryld','perc','et','ecanopy','eplant','esoil','surq_cont',
             'cn','sw_init','sw_final','sw_ave','sw_300','sno_init','sno_final','snopack',
             'pet','qtile','irr','surq_runon','latq_runon','overbank','surq_cha','surq_res',
             'surq_ls','latq_cha','latq_res','latq_ls','gwtranq','satex','satex_chan','sw_change',
             'lagsurf','laglatq','lagsatex','wet_evap')

# legge il file come tabella
df = read.delim(filename, header = FALSE, sep = "", dec = ".", skip =3, col.names= colNames)

# seleziona solo le righe che corrispondono a hru_name
sub_df = subset(df,df$name==hru_name)
sub_df = subset(sub_df,sub_df$yr>=1994)

# crea un campo con le date
sub_ts= ISOdatetime(sub_df$yr,sub_df$mon,sub_df$day,12,0,0)

if (analysis_type == 'evapotranspiration'){
  # crea una tabella con i campi di interesse
  alldata = data.frame(date = sub_ts,
                       et=sub_df$et,
                       ecanopy=sub_df$ecanopy,
                       eplant=sub_df$eplant,
                       esoil = sub_df$esoil)
  # crea un plot
  p <- ggplot(alldata, aes(date)) +
    geom_line(aes(y = et, colour  = "et")) +
    geom_line(aes(y = ecanopy, colour  = "ecanopy")) +
    geom_line(aes(y = eplant, colour  = "eplant")) +
    geom_line(aes(y = esoil, colour  = "esoil")) +
    ylab("water (mm)") +
    ggtitle(hru_name) +
    theme_ipsum()

}

if (analysis_type == 'snow'){
  # crea una tabella con i campi di interesse
  alldata = data.frame(date = sub_ts,prec=sub_df$precip,
                       snowfall=sub_df$snofall,
                       snowmelt=sub_df$snomlt,
                       snowpack = sub_df$snopack)
  # crea un plot
  p <- ggplot(alldata, aes(date)) +
    geom_line(aes(y = prec, colour  = "prec")) +
    geom_line(aes(y = snowfall, colour  = "snowfall")) +
    geom_line(aes(y = snowmelt, colour  = "snowmelt")) +
    geom_line(aes(y = snowpack, colour  = "snowpack")) +
    ylab("water (mm)") +
    ggtitle(hru_name) +
    theme_ipsum()

}

if (analysis_type == 'water_balance'){
  # crea una tabella con i campi di interesse
  alldata = data.frame(date = sub_ts,
                       prec=sub_df$precip,
                       surq_gen=sub_df$surq_gen,
                       latq=sub_df$latq,
                       wateryld = sub_df$wateryld,
                       perc = sub_df$perc,
                       et = sub_df$et,
                       irr = sub_df$irr)
  # crea un plot
  p <- ggplot(alldata, aes(date)) +
    geom_line(aes(y = prec, colour  = "prec")) +
    geom_line(aes(y = surq_gen, colour  = "surq_gen")) +
    geom_line(aes(y = latq, colour  = "latq")) +
    geom_line(aes(y = wateryld, colour  = "wateryld")) +
    geom_line(aes(y = perc, colour  = "perc")) +
    geom_line(aes(y = et, colour  = "et")) +
    geom_line(aes(y = irr, colour  = "irr")) +
    ylab("water (mm)") +
    ggtitle(hru_name) +
    theme_ipsum()

}

if (analysis_type == 'cn'){
  # crea una tabella con i campi di interesse
  alldata = data.frame(date = sub_ts,
                       prec=sub_df$precip,
                       surq_cont=sub_df$surq_cont,
                       cn=sub_df$cn)
  # crea un plot
  p <- ggplot(alldata, aes(date)) +
    geom_line(aes(y = prec, colour  = "prec")) +
    geom_line(aes(y = surq_cont, colour  = "surq_cont")) +
    geom_line(aes(y = cn, colour  = "cn")) +
    ylab("water (mm)") +
    ggtitle(hru_name) +
    theme_ipsum()

}


if (analysis_type == 'soil_balance'){
  # crea una tabella con i campi di interesse
  alldata = data.frame(date = sub_ts,
                       cn=sub_df$cn,
                       sw_init=sub_df$sw_init,
                       sw_final=sub_df$sw_final,
                       sw_ave = sub_df$sw_ave,
                       sw_300 = sub_df$sw_300)
  # crea un plot
  p <- ggplot(alldata, aes(date)) +
    geom_line(aes(y = cn, colour  = "cn")) +
    geom_line(aes(y = sw_init, colour  = "sw_init")) +
    geom_line(aes(y = sw_final, colour  = "sw_final")) +
    geom_line(aes(y = sw_ave, colour  = "sw_ave")) +
    geom_line(aes(y = sw_300, colour  = "sw_300"))+
    ylab("water (mm)") +
    ggtitle(hru_name) +
    theme_ipsum()

}

if (analysis_type == 'lat_flow'){
  # crea una tabella con i campi di interesse
  alldata = data.frame(date = sub_ts,
                       surq_runon=sub_df$surq_runon, # surface runoff from upland landscape units
                       latq_runon=sub_df$latq_runon, # lateral soil flow from upland landscape units
                       surq_cha=sub_df$surq_cha, # mm H2O |surface runoff flowing into channels
                       surq_res = sub_df$surq_res,# mm H2O |surface runoff flowing into reservoirs
                       surq_ls = sub_df$surq_ls,# mm H2O |surface runoff flowing into a landscape element
                       latq_cha = sub_df$latq_cha, # mm H2O |lateral soil flow into channels
                       latq_res = sub_df$latq_res, # mm H2O |lateral soil flow into reservoirs
                       latq_ls = sub_df$latq_ls # mm H2O |lateral soil flow into a landscape element
                       )
  # crea un plot
  p <- ggplot(alldata, aes(date)) +
    geom_line(aes(y = surq_runon, colour  = "surq_runon")) +
    geom_line(aes(y = latq_runon, colour  = "latq_runon")) +
    
    geom_line(aes(y = surq_cha, colour  = "surq_cha")) +
    geom_line(aes(y = surq_res, colour  = "surq_res")) +
    geom_line(aes(y = surq_ls, colour  = "surq_ls"))+
    
    geom_line(aes(y = latq_cha, colour  = "latq_cha")) +
    geom_line(aes(y = latq_res, colour  = "latq_res")) +
    geom_line(aes(y = latq_ls, colour  = "latq_ls"))+
    
    ylab("water (mm)") +
    ggtitle(hru_name) +
    theme_ipsum()
}

# rende il plot interattivo
p <- ggplotly(p)
p
