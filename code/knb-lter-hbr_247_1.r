# Package ID: knb-lter-hbr.247.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Measurements of N cycling dynamics in the soils of the Ice Storm Experiment (ISE) plots at the Hubbard Brook Experimental Forest.
# Data set creator:  Peter M Groffman -  
# Data set creator:  Julie Weitzman -  
# Data set creator:  Lisa Martel -  
# Contact:    - Information Manager Hubbard Brook Ecosystem Study  - hbr-im@lternet.edu
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@Virginia.edu 
#
#install package tidyverse if not already installed
if(!require(tidyverse)){ install.packages("tidyverse") }  
library("tidyverse") 
infile1 <- trimws("https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/247/1/465ebbfa86f23afe0d1c1eeb3fce17bf") 
infile1 <-sub("^https","http",infile1)
# This creates a tibble named: dt1 
	dt1 <-read_delim(infile1  
                ,delim=","   
                ,skip=1 
                    ,quote='"'  
                    , col_names=c( 
                        "Project",   
                        "Date",   
                        "Year",   
                        "TrtYr",   
                        "Season",   
                        "Treatment",   
                        "Plot",   
                        "Horizon",   
                        "BIOC",   
                        "RESPC",   
                        "BION",   
                        "NO3",   
                        "NH4",   
                        "NIT",   
                        "MIN",   
                        "DEA",   
                        "H2O",   
                        "OM",   
                        "inNIT",   
                        "inMIN"   ), 
                    col_types=list( 
                        col_character(),  
                        col_date("%Y-%m-%d"),  
                        col_character(),  
                        col_character(),  
                        col_character(),  
                        col_character(),  
                        col_character(),  
                        col_character(), 
                        col_number() , 
                        col_number() , 
                        col_number() , 
                        col_number() , 
                        col_number() , 
                        col_number() , 
                        col_number() , 
                        col_number() , 
                        col_number() , 
                        col_number() , 
                        col_number() , 
                        col_number() ), 
                        na=c(" ",".","NA")  )
                        
                    
# Convert Missing Values to NA for individual vectors 
dt1$BIOC <- ifelse((trimws(as.character(dt1$BIOC))==trimws("-9999.99")),NA,dt1$BIOC)               
suppressWarnings(dt1$BIOC <- ifelse(!is.na(as.numeric("-9999.99")) & (trimws(as.character(dt1$BIOC))==as.character(as.numeric("-9999.99"))),NA,dt1$BIOC))
dt1$RESPC <- ifelse((trimws(as.character(dt1$RESPC))==trimws("-9999.99")),NA,dt1$RESPC)               
suppressWarnings(dt1$RESPC <- ifelse(!is.na(as.numeric("-9999.99")) & (trimws(as.character(dt1$RESPC))==as.character(as.numeric("-9999.99"))),NA,dt1$RESPC))
dt1$BION <- ifelse((trimws(as.character(dt1$BION))==trimws("-9999.99")),NA,dt1$BION)               
suppressWarnings(dt1$BION <- ifelse(!is.na(as.numeric("-9999.99")) & (trimws(as.character(dt1$BION))==as.character(as.numeric("-9999.99"))),NA,dt1$BION))
dt1$NO3 <- ifelse((trimws(as.character(dt1$NO3))==trimws("-9999.99")),NA,dt1$NO3)               
suppressWarnings(dt1$NO3 <- ifelse(!is.na(as.numeric("-9999.99")) & (trimws(as.character(dt1$NO3))==as.character(as.numeric("-9999.99"))),NA,dt1$NO3))
dt1$NH4 <- ifelse((trimws(as.character(dt1$NH4))==trimws("-9999.99")),NA,dt1$NH4)               
suppressWarnings(dt1$NH4 <- ifelse(!is.na(as.numeric("-9999.99")) & (trimws(as.character(dt1$NH4))==as.character(as.numeric("-9999.99"))),NA,dt1$NH4))
dt1$NIT <- ifelse((trimws(as.character(dt1$NIT))==trimws("-9999.99")),NA,dt1$NIT)               
suppressWarnings(dt1$NIT <- ifelse(!is.na(as.numeric("-9999.99")) & (trimws(as.character(dt1$NIT))==as.character(as.numeric("-9999.99"))),NA,dt1$NIT))
dt1$MIN <- ifelse((trimws(as.character(dt1$MIN))==trimws("-9999.99")),NA,dt1$MIN)               
suppressWarnings(dt1$MIN <- ifelse(!is.na(as.numeric("-9999.99")) & (trimws(as.character(dt1$MIN))==as.character(as.numeric("-9999.99"))),NA,dt1$MIN))
dt1$DEA <- ifelse((trimws(as.character(dt1$DEA))==trimws("-9999.99")),NA,dt1$DEA)               
suppressWarnings(dt1$DEA <- ifelse(!is.na(as.numeric("-9999.99")) & (trimws(as.character(dt1$DEA))==as.character(as.numeric("-9999.99"))),NA,dt1$DEA))
dt1$H2O <- ifelse((trimws(as.character(dt1$H2O))==trimws("-9999.99")),NA,dt1$H2O)               
suppressWarnings(dt1$H2O <- ifelse(!is.na(as.numeric("-9999.99")) & (trimws(as.character(dt1$H2O))==as.character(as.numeric("-9999.99"))),NA,dt1$H2O))
dt1$OM <- ifelse((trimws(as.character(dt1$OM))==trimws("-9999.99")),NA,dt1$OM)               
suppressWarnings(dt1$OM <- ifelse(!is.na(as.numeric("-9999.99")) & (trimws(as.character(dt1$OM))==as.character(as.numeric("-9999.99"))),NA,dt1$OM))
dt1$inNIT <- ifelse((trimws(as.character(dt1$inNIT))==trimws("-9999.99")),NA,dt1$inNIT)               
suppressWarnings(dt1$inNIT <- ifelse(!is.na(as.numeric("-9999.99")) & (trimws(as.character(dt1$inNIT))==as.character(as.numeric("-9999.99"))),NA,dt1$inNIT))
dt1$inMIN <- ifelse((trimws(as.character(dt1$inMIN))==trimws("-9999.99")),NA,dt1$inMIN)               
suppressWarnings(dt1$inMIN <- ifelse(!is.na(as.numeric("-9999.99")) & (trimws(as.character(dt1$inMIN))==as.character(as.numeric("-9999.99"))),NA,dt1$inMIN))
                    
                
# Observed issues when reading the data. An empty list is good!
problems(dt1) 
# Here is the structure of the input data tibble: 
glimpse(dt1) 
# And some statistical summaries of the data 
summary(dt1) 
# Get more details on character variables
                     
summary(as.factor(dt1$Project)) 
summary(as.factor(dt1$TrtYr)) 
summary(as.factor(dt1$Season)) 
summary(as.factor(dt1$Treatment)) 
summary(as.factor(dt1$Plot)) 
summary(as.factor(dt1$Horizon))




