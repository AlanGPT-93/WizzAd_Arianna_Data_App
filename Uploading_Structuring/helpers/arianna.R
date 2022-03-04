get_arianna_data <- function(my_data){
  
  # change columns names
  colnames(my_data) <- my_data %>% colnames() %>% str_replace("\\\\","_") %>% 
    str_replace(" ","_")
  
  # Change Advertiser's name
  my_data$Advertiser <- my_data$Advertiser %>% 
    str_replace_all(c("LIBERTY CABLEVISION" = "LIBERTY",
                      "AMERICA MOVIL" = "CLARO","DISH NETWORK" = "DISH", "HOME FI" = "AERONET"))
  
  #aux_b <- my_data$`Brand_Context_Variables` %>% str_detect("LIBERTY / AT&T")
  aux_b <- my_data$Brand_Context %>% str_detect("LIBERTY / AT&T")
  my_data$Advertiser[aux_b] <- "AT&T"
  
  
  # Change Brand Context
  a <- c('AT&T B: BUSINESS SERVICES', 'DIRECTV D: TV OFFERTS', 'DIRECTV D: HBO MAX',	'AT&T B',	
         'AT&T B: DATA PLAN', 'AT&T B: OFFERTS',	'AT&T B: PREPAID',	'CLARO WIRELESS B',	
         'CLARO B: INTERNET', 'CLARO B: LIFELINE', 'CLARO B: ALL TELECOM PRODS',	
         'CLARO B: OFFERTS', 'CLARO B: PREPAID',	'DIRECTV: TV OFFERTS', 'DIRECTV: PROGRAMACION AD', 'DIRECTV',	
         'DISH PUERTO RICO', 'DISH PUERTO RICO: HOPPER',	'HUGHESNET', 'LIBERTY: BUSINESS', 'LIBERTY: GO',	
         'LIBERTY CABLEVISION', 'LIBERTY: ALL TELECOM PROD OFFERTS', 'LIBERTY / AT&T',	
         'T MOBILE C: BUSINESS SERVICES',	'T MOBILE C', 'T MOBILE: OFFERT', 'T MOBILE C: ACCESORIOS',
         'WORLDNET','LIBERTY MOBILITY: PREPAID','LIBERTY MOBILITY: OFFERT', 'LIBERTY MOBILITY',
         'HOME FI','LIBERTY MOBILITY: ACCESORIOS','LIBERTY MOBILITY: DATA PLAN')
  
  b <- c('AT&T: BUSINESS','AT&T: CABLE',	'AT&T: CABLE',	'AT&T: INSTITUTIONAL',	
         'AT&T: POST PAID', 'AT&T: POST PAID', 'AT&T: PREPAID',	'CLARO: INSTITUTIONAL',	
         'CLARO: INTERNET', 'CLARO: LIFELINE', 'CLARO: MULTIPROD', 'CLARO: POST PAID',	
         'CLARO: PREPAID', 'DIRECTV: CABLE', 'DIRECTV: PROGRAM', 'DIRECTV: CABLE',
         'DISH: INSTITUTIONAL', 'DISH NETWORK: HOPPER', 'HUGHESNET: INTERNET',	'LIBERTY: BUSINESS',	'LIBERTY: GO',	
         'LIBERTY: INSTITUTIONAL', 'LIBERTY: MULTIPROD',	'AT&T: CO BRAND', 'T-MOBILE: BUSINESS',	
         'T-MOBILE: INSTITUTIONAL', 'T-MOBILE: POST PAID', 'T-MOBILE: POST PAID',
         'WORLDNET: INTERNET','LIBERTY: PREPAID','LIBERTY: POST PAID', 'LIBERTY MOBILITY',
         'AERONET: INTERNET', 'LIBERTY: POST PAID','LIBERTY: POST PAID')
  
  # Create a DF with a & b
  #data_brand_aux <- data.frame("Brand_Context_Variables" = a, "Brand_Change" = b)
  data_brand_aux <- data.frame("Brand_Context" = a, "Brand_Change" = b)
  
  # Join my_data with data_brand_aux on Brand_Context_Variables
  #my_data <- my_data %>% left_join(data_brand_aux, by = "Brand_Context_Variables")
  my_data <- my_data %>% left_join(data_brand_aux, by = "Brand_Context")
  
  # change col names to upper and adding 2 new columns
  my_data$Category <- ""
  my_data$DUPLICATED <- "NO"
  
  
  ################################ Repeat in another category
  ## Create Categories
  
  #### Mobility
  
  # aux <- my_data$Brand_Change %in% c("AT&T: INSTITUTIONAL", "AT&T: INTERNET",'AT&T: FIRSTNET',
  #                             "CLARO: INSTITUTIONAL", "CLARO: INTERNET",
  #                             'AT&T: BUSINESS', 'CLARO: BUSINESS', 
  #                             'LIBERTY: BUSINESS','T-MOBILE: BUSINESS')
  
  aux <- my_data$Brand_Change %in% c() # "AT&T: INTERNET",'AT&T: BUSINESS', 'CLARO: BUSINESS', 'T-MOBILE: BUSINESS'
  data_aux <- my_data[aux,]
  
  data_aux$Category <- "Mobility"
  data_aux$DUPLICATED <- "SI"
  
  #### Business
  aux_2 <- my_data$Brand_Change %in% c() #"NEPTUNO: INTERNET", "WORLDNET: INTERNET"
  
  data_aux_2 <- my_data[aux_2,]
  
  data_aux_2$Category <- "Business"
  data_aux_2$DUPLICATED <- "SI"
  
  #### Residence
  # aux_3 <- my_data$Brand_Change %in% c('AT&T: FIRSTNET')
  # 
  # data_aux_3 <- my_data[aux_3,]
  # 
  # data_aux_3$CATEGORY <- "Residence"
  # data_aux_3$DUPLICATED <- "SI"
  
  ################################## PRINCIPAL CATEGORY
  ### INSTITUTIONAL
  
  institutional <- c("CLARO: INSTITUTIONAL", "LIBERTY: INSTITUTIONAL", # eran residence
                     'T-MOBILE: INSTITUTIONAL',"AT&T: INSTITUTIONAL",'AT&T: CO BRAND', # eran mobility
                     'LIBERTY MOBILITY')   
  institutional_f <- my_data$Brand_Change %in% institutional
  
  my_data[institutional_f,]$Category <- "Institutional"
  
  
  ### Residence
  
  # residence <- c('AT&T: FIRSTNET','AT&T: CABLE', 'AT&T: INSTITUTIONAL', 'AT&T: INTERNET', 'AT&T: LIFELINE', 'CLARO: INSTITUTIONAL',
  #                'CLARO: INTERNET', 'CLARO: LIFELINE', 'CLARO: MULTIPROD', 'DIRECTV: CABLE', 'DIRECTV: INSTITUTIONAL',
  #                'DIRECTV: PROGRAM','DISH: CABLE', 'DISH NETWORK: HOPPER', 'DISH: INSTITUTIONAL', 'HUGHESNET: INTERNET',
  #                'LIBERTY: CABLE', 'LIBERTY: GO', 'LIBERTY: INSTITUTIONAL', 'LIBERTY: INTERNET', 'LIBERTY: LIFELINE',
  #                'LIBERTY: MULTIPROD', 'LIBERTY: ON DEMAND', 'LIBERTY: PAY PER VIEW', 'LIBERTY: PROGRAMACION',
  #                'NEPTUNO: INTERNET', 'WORLDNET: INTERNET') , 'WORLDNET: INTERNET'
  # ,'CLARO: INSTITUTIONAL', 'LIBERTY: INSTITUTIONAL'
  residence <- c('AT&T: INTERNET', 'AT&T: CABLE',
                 'CLARO: INTERNET', 'CLARO: LIFELINE', 'CLARO: MULTIPROD', 'DIRECTV: CABLE', 'DIRECTV: INSTITUTIONAL',
                 'DIRECTV: PROGRAM', 'DISH NETWORK: HOPPER', 'DISH: INSTITUTIONAL', 'HUGHESNET: INTERNET',
                 'LIBERTY: CABLE', 'LIBERTY: GO', 'LIBERTY: INTERNET', 'LIBERTY: LIFELINE',
                 'LIBERTY: MULTIPROD', 'LIBERTY: ON DEMAND', 'LIBERTY: PAY PER VIEW', 'LIBERTY: PROGRAMACION','LIBERTY: HUB TV',
                 'NEPTUNO: INTERNET','AT&T: DTV HBO MAX',
                 'AT&T: DTV PPV', 'AT&T: DTV PROGRAM', 'AT&T: MULTIPROD', 'AERONET: INTERNET',
                 'CLARO: CABLE', 'DISH NETWORK: ANYWHERE', 'DISH NETWORK: CABLE', 'DISH NETWORK: MULTIPROD',
                 'LIBERTY: EVERYWHERE', 'LIBERTY: PREMIUM CHANNELS','CLARO: TV',
                 'WORLDNET: INTERNET')
  
  residence_f <- my_data$Brand_Change %in% residence
  
  my_data[residence_f,]$Category <- "Residence"
  
  ### Mobility
  # mobility <- c('AT&T: CO BRAND', 'AT&T: POST PAID', 'AT&T: PREPAID', 'BOOST: PREPAID',
  #               'CLARO: POST PAID', 'CLARO: PREPAID', 'T-MOBILE: INSTITUTIONAL', 'T-MOBILE: INTERNET',
  #               'T-MOBILE: POST PAID', 'T-MOBILE: PREPAID')
  # 'AT&T: INSTITUTIONAL', 'T-MOBILE: INSTITUTIONAL','AT&T: CO BRAND'
  mobility <- c( 'AT&T: FIRSTNET','AT&T: LIFELINE',
                 'AT&T: POST PAID', 'AT&T: PREPAID', 'BOOST: PREPAID',
                 'CLARO: POST PAID', 'CLARO: PREPAID',
                 'T-MOBILE: INTERNET',
                 'T-MOBILE: POST PAID', 'T-MOBILE: PREPAID','BOOST: INSTITUTIONAL',
                 'LIBERTY MOBILITY: OFFERT','LIBERTY: PREPAID','LIBERTY: POST PAID',
                 'BOOST: INSTITUTIONAL','SPRINT: POST PAID', 'SPRINT: INSTITUTIONAL')
  
  mobility_f <- my_data$Brand_Change %in% mobility
  
  my_data[mobility_f,]$Category <- "Mobility"
  
  
  ## Business
  
  # business <- c('AERONET: INTERNET','AT&T: BUSINESS', 'CLARO: BUSINESS', 'LIBERTY: BUSINESS',
  #               'T-MOBILE: BUSINESS','WORLDNET: INTERNET')
  business <- c('AT&T: BUSINESS', 'CLARO: BUSINESS', 'LIBERTY: BUSINESS',
                'T-MOBILE: BUSINESS','AERONET: BUSINESS', 'DIRECTV: BUSINESS', 
                'LIBERTY: BUSINESS MULTIPROD','FUSE TELECOM',
                'WORLDNET: BUSINESS')
  
  business_f <- my_data$Brand_Change %in% business
  
  my_data[business_f,]$Category <- "Business"
  
  ## Union
  
  copetitive <- my_data %>% bind_rows(data_aux) %>% bind_rows(data_aux_2)# %>% bind_rows(data_aux_3)
  
  ## Enhanced Category
  
  copetitive$enhanced_category <- ""
  
  ### Business
  aux_b <- str_detect(copetitive$Target, "PEOPLE 25") & str_detect(copetitive$Category, "Business")
  aux_b %>% sum()
  copetitive$enhanced_category[aux_b] <- "Business"
  
  ### Mobility
  #aux_b <- str_detect(copetitive$Target, "Mobility") & str_detect(copetitive$Category, "Mobility")
  aux_b <- str_detect(copetitive$Target, "PEOPLE 18+") & str_detect(copetitive$Category, "Mobility")
  aux_b %>% sum()
  copetitive$enhanced_category[aux_b] <- "Mobility"
  
  ## Residence
  aux_b <- str_detect(copetitive$Target, "PEOPLE 25") & str_detect(copetitive$Category, "Residence")
  aux_b %>% sum()
  copetitive$enhanced_category[aux_b] <- "Residence"
  
  ## Institutional
  aux_b <- str_detect(copetitive$Target, "PEOPLE 18+") & str_detect(copetitive$Category, "Institutional")
  aux_b %>% sum()
  copetitive$enhanced_category[aux_b] <- "Institutional"
  
  return(copetitive)
}