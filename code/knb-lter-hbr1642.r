# Package ID: knb-lter-hbr.164.2 Cataloging System:https://pasta.edirepository.org.
# Data set title: Net N mineralization in the MELNHE study at Hubbard     Brook Experimental Forest, Bartlett Experimental Forest and     Jeffers Brook, central NH USA, 2008 - present.
# Data set creator:  Melany Fisk - Miami University 
# Metadata Provider:    - Hubbard Brook Experimental Forest
      LTER 
# Contact:    - Information Manager, Hubbard Brook
      LTER   - 
      hbr-im@lternet.edu
# Stylesheet for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@Virginia.edu 
#
#install package tidyverse if not already installed
if(!require(tidyverse)){ install.packages("tidyverse") }  
library("tidyverse") 
infile1 <- trimws("https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/164/2/a282b40a750d25549ed794b824032198") 
infile1 <-sub("^https","http",infile1)
# This creates a tibble named: dt1 
	dt1 <-read_delim(infile1  
                ,delim=","  
                ,skip=1 
                    , col_names=c( 
                        "Site",   
                        "Age",   
                        "Stand",   
                        "Plot",   
                        "Treatment",   
                        "Year",   
                        "Date",   
                        "Days_Inc",   
                        "OeNmin",   
                        "OaNmin",   
                        "MinNmin",   
                        "OeNitr",   
                        "OaNitr",   
                        "MinNitr",   
                        "OeNO3i",   
                        "OaNO3i",   
                        "MinNO3i",   
                        "OeTNi",   
                        "OaTNi",   
                        "MinTNi"   ), 
                    col_types=list( 
                        col_character(),  
                        col_character(),  
                        col_character(),  
                        col_character(),  
                        col_character(), 
                        col_character(),  
                        col_date("%Y-%m-%d"),  
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
                        col_number() , 
                        col_number() ), 
                        na=c(" ",".","NA")  )
                        
                    
# Convert Missing Values to NA for individual vectors 
dt1$OeNmin <- ifelse((trimws(as.character(dt1$OeNmin))==trimws("-999.99")),NA,dt1$OeNmin)               
suppressWarnings(dt1$OeNmin <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$OeNmin))==as.character(as.numeric("-999.99"))),NA,dt1$OeNmin))
dt1$OaNmin <- ifelse((trimws(as.character(dt1$OaNmin))==trimws("-999.99")),NA,dt1$OaNmin)               
suppressWarnings(dt1$OaNmin <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$OaNmin))==as.character(as.numeric("-999.99"))),NA,dt1$OaNmin))
dt1$MinNmin <- ifelse((trimws(as.character(dt1$MinNmin))==trimws("-999.99")),NA,dt1$MinNmin)               
suppressWarnings(dt1$MinNmin <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$MinNmin))==as.character(as.numeric("-999.99"))),NA,dt1$MinNmin))
dt1$OeNitr <- ifelse((trimws(as.character(dt1$OeNitr))==trimws("-999.99")),NA,dt1$OeNitr)               
suppressWarnings(dt1$OeNitr <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$OeNitr))==as.character(as.numeric("-999.99"))),NA,dt1$OeNitr))
dt1$OaNitr <- ifelse((trimws(as.character(dt1$OaNitr))==trimws("-999.99")),NA,dt1$OaNitr)               
suppressWarnings(dt1$OaNitr <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$OaNitr))==as.character(as.numeric("-999.99"))),NA,dt1$OaNitr))
dt1$MinNitr <- ifelse((trimws(as.character(dt1$MinNitr))==trimws("-999.99")),NA,dt1$MinNitr)               
suppressWarnings(dt1$MinNitr <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$MinNitr))==as.character(as.numeric("-999.99"))),NA,dt1$MinNitr))
dt1$OeNO3i <- ifelse((trimws(as.character(dt1$OeNO3i))==trimws("-999.99")),NA,dt1$OeNO3i)               
suppressWarnings(dt1$OeNO3i <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$OeNO3i))==as.character(as.numeric("-999.99"))),NA,dt1$OeNO3i))
dt1$OaNO3i <- ifelse((trimws(as.character(dt1$OaNO3i))==trimws("-999.99")),NA,dt1$OaNO3i)               
suppressWarnings(dt1$OaNO3i <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$OaNO3i))==as.character(as.numeric("-999.99"))),NA,dt1$OaNO3i))
dt1$MinNO3i <- ifelse((trimws(as.character(dt1$MinNO3i))==trimws("-999.99")),NA,dt1$MinNO3i)               
suppressWarnings(dt1$MinNO3i <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$MinNO3i))==as.character(as.numeric("-999.99"))),NA,dt1$MinNO3i))
dt1$OeTNi <- ifelse((trimws(as.character(dt1$OeTNi))==trimws("-999.99")),NA,dt1$OeTNi)               
suppressWarnings(dt1$OeTNi <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$OeTNi))==as.character(as.numeric("-999.99"))),NA,dt1$OeTNi))
dt1$OaTNi <- ifelse((trimws(as.character(dt1$OaTNi))==trimws("-999.99")),NA,dt1$OaTNi)               
suppressWarnings(dt1$OaTNi <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$OaTNi))==as.character(as.numeric("-999.99"))),NA,dt1$OaTNi))
dt1$MinTNi <- ifelse((trimws(as.character(dt1$MinTNi))==trimws("-999.99")),NA,dt1$MinTNi)               
suppressWarnings(dt1$MinTNi <- ifelse(!is.na(as.numeric("-999.99")) & (trimws(as.character(dt1$MinTNi))==as.character(as.numeric("-999.99"))),NA,dt1$MinTNi))
                    
                
# Observed issues when reading the data. An empty list is good!
problems(dt1) 
# Here is the structure of the input data tibble: 
glimpse(dt1) 
# And some statistical summaries of the data 
summary(dt1) 
# Get more details on character variables
                     
summary(as.factor(dt1$Site)) 
summary(as.factor(dt1$Age)) 
summary(as.factor(dt1$Stand)) 
summary(as.factor(dt1$Plot)) 
summary(as.factor(dt1$Treatment))




