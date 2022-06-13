# Info --------------------------------------
# Matthew Vaughan, Lake Champlain Basin Program Chief Scientist
# This code processes realtime data data from monitoring buoys, USGS, 
# other sources to prepare it for data.lcbp.org

# Setup ---------------------------------------
library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(weathermetrics)
library(RCurl)
library(magick)

ftp_info <- "data/ftp_info.csv" %>%
  read_csv()

ftp_credentials <- ftp_info %>%
  pluck("credentials", 1)

# Lake level ----------------------------------
lake_level_station_info <- "data/lake_level_station_info.csv" %>%
  read_csv() %>%
  mutate(gage_number = gage_number %>% # Add leading zero to all gage codes
           paste0("0", .)) 

lake_level_day_window <- 120

param_code <- "62614" # Lake water surface elevation above NGVD 1929, feet
# param_code <- "62615" # Lake water surface elevation above NAVD 1988, feet
time_zone <- "America/New_York"

end <- Sys.Date()
start <-  end - duration(lake_level_day_window, units = "days")

lake_level <- lake_level_station_info %>%
  select(c(station, gage_number)) %>%
  mutate(level_data = map(.x = gage_number,
                          .f = ~readNWISuv(siteNumber = .x,
                                           startDate = start,
                                           endDate = end,
                                           parameterCd = param_code,
                                           tz = time_zone) %>%
                            rename(timestamp = dateTime,
                                   elevation_ft = X_62614_00000) %>%
                            select(c(timestamp,
                                     elevation_ft)))) %>%
  unnest(level_data)

 # write it
 lake_level %>%
   write_csv("plotdata/lake_level.csv")

# Lake temperature -----------------------------
 
 # gather station info
 lake_temp_station_info <- "data/lake_temp_station_info.csv" %>%
   read_csv() %>%
   mutate(gage_number = gage_number %>% # Add leading zero to all gage codes
            paste0("0", .)) 
 # enter plot window duration
 lake_temp_day_window <- 120
 # set start and end dates
 end <- Sys.Date()
 start <-  end - duration(lake_temp_day_window, units = "days")
 
 # USGS parameter code and time zone
 param_code <- "00010" # water temperature in degC
 time_zone <- "America/New_York"
 
 # gather data and create tibble
 lake_temp <- lake_temp_station_info %>%
   select(c(station, gage_number)) %>%
   mutate(temp_data = map(.x = gage_number,
                          .f = ~readNWISuv(siteNumber = .x,
                                           startDate = start,
                                           endDate = end,
                                           parameterCd = param_code,
                                           tz = time_zone) %>%
                            rename(timestamp = dateTime,
                                   water_temp_degC = X_00010_00000) %>%
                            select(c(timestamp,
                                     water_temp_degC)))) %>%
   unnest(temp_data) %>%
   mutate(water_temp_degF = water_temp_degC %>% # create column for Fahrenheit
            celsius.to.fahrenheit())
 
 # write it
 lake_temp %>%
   write_csv("plotdata/lake_temp.csv")

# Tributaries ----------------------------------
 trib_station_info <- "data/trib_station_info.csv" %>%
   read_csv() %>%
   mutate(gage_number = gage_number %>% # Add leading zero to all gage codes
            paste0("0", .)) 
 
 trib_day_window <- 120
 
 param_code <- "00060" # discharge in cfs
 time_zone <- "America/New_York"
 
 end <- Sys.Date()
 start <-  end - duration(trib_day_window, units = "days")
 
 tribq <- trib_station_info %>%
   select(c(trib, gage_number)) %>%
   mutate(qdata = map(.x = gage_number,
                      .f = ~readNWISuv(siteNumber = .x,
                                       startDate = start,
                                       endDate = end,
                                       parameterCd = param_code,
                                       tz = time_zone) %>%
                        rename(timestamp = dateTime,
                               discharge_cfs = X_00060_00000) %>%
                        mutate(discharge_cms = discharge_cfs * 0.0283168,
                               timestamp = timestamp %>% 
                                           as_datetime(tz = "America/New_York")) %>%
                        select(c(timestamp,
                                 discharge_cfs,
                                 discharge_cms)))) %>%
   unnest(qdata)
 
 # write it
 tribq %>%
 write_csv("plotdata/tribq.csv")
 
# Buoys -----------------------------------------
 ## Lamoille ------------------------------------
 lamoille <- ftp_info %>% # start with ftp info
   pluck("address", 2) %>% # pull out lamoille address
   getURL(userpwd = ftp_credentials) %>% # grab file
   read_csv(skip = 1) %>% # skip header
   rename(timestamp = `Time America/New_York UTC-04:00`) %>%
   drop_na() %>%
   mutate(timestamp = timestamp %>%
                      mdy_hms()) %>%
   mutate_if(is.character, as.numeric) %>% # convert all those characters to numeric
   filter(timestamp > ymd_hms("2022-05-06 12:30:00")) %>% # start after deployment date
   select(-`pH mV`) %>% # remove pH_mV variable for now
   rename(temp_degC = Temperature,
          sp_cond_uScm = `Sp Cond`,
          cond_uScm = Conductivity,
          odo_mgL = ODO,
          odo_pct_sat = ODOSat,
          turb_fnu = `Turbidity FNU`,
          nitrate_mgL = `NO3-`) %>%
   filter(timestamp <= ymd_hms("2022-05-20 09:30:00") | # removing measurements during maintenance manually for now; planning to write a function that does this better. 
            timestamp >= ymd_hms("2022-05-20 11:30:00")) %>%
   mutate(nitrate_mgL = case_when(timestamp >= ymd_hms("2022-06-02 10:00:00") ~ nitrate_mgL, # remove nitrate values from before it was properly calibrated
                                  TRUE ~ NA_real_)) %>%
   mutate_if(is.numeric, funs(if_else(. < 0, 0, .))) # replace negatives with zero.
 
 # write it
 lamoille %>%
   write_csv("plotdata/lamoille.csv")
 
 ## Malletts ------------------------------------------
 malletts <- ftp_info %>%
   pluck("address", 3) %>% # pull out lamoille address
   getURL(userpwd = ftp_credentials) %>% # grab file
   read_csv(skip = 1) %>% # skip header
   rename(timestamp = `Time America/New_York UTC-04:00`) %>%
   drop_na() %>%
   mutate(timestamp = timestamp %>%
                      mdy_hms()) %>%
   mutate_if(is.character, as.numeric) %>% # convert all those characters to numeric
   filter(timestamp >= ymd_hms("2022-05-31 14:30:00")) %>% # start after deployment date
   rename(temp_01m = Temp00,
          temp_02m = Temp01,
          temp_03m = Temp02,
          temp_04m = Temp03,
          temp_05m = Temp04,
          temp_06m = Temp05,
          temp_07m = Temp06,
          temp_08m = Temp07,
          temp_09m = Temp08,
          temp_10m = Temp09,
          temp_11m = Temp10,
          temp_12m = Temp11,
          temp_13m = Temp12,
          temp_14m = Temp13,
          temp_15m = Temp14,
          temp_16m = Temp15,
          temp_17m = Temp16,
          temp_18m = Temp17,
          temp_19m = Temp18,
          temp_20m = Temp19,
          temp_21m = Temp20,
          temp_22m = Temp21,
          temp_23m = Temp22,
          temp_24m = Temp23,
          temp_25m = Temp24,
          temp_26m = Temp25,
          temp_27m = Temp26,
          temp_28m = Temp27,
          temp_degC = Temperature,
          sp_cond_uScm = `Sp Cond`,
          cond_uScm = Conductivity,
          odo_mgL = ODO,
          odo_pct_sat = ODOSat,
          turb_fnu = `Turbidity FNU`,
          chlorophyll_rfu = `Chlorophyll RFU`,
          chlorophyll_ugL = `Chlorophyll`,
          phycocyanin_rfu = `BGA-Phycocyanin RFU`,
          phycocyanin_ugL = `BGA-PC`,
          nitrate_mgL = `NO3-`,
          rel_atm_pressure_hPa = `Rel. Barometric Pressure`,
          air_temp_degC = `Air Temperature`,
          rel_humidity_pct = `Relative Humidity`,
          wind_speed_mps = `Wind Speed`,
          max_wind_speed_mps = `Max Wind Sp`,
          wind_direction_deg = `Wind Direction`,
          solar_radiation_Wm2 = `Solar Rad`,
          heading_deg = `Heading`) %>%
   mutate_if(is.numeric, funs(if_else(. < 0, 0, .))) %>% # replace negatives with zero.
   filter(timestamp <= ymd_hms("2022-06-09 09:15:00") | # removing error measurements manually for now; planning to write a function that does this better. 
            timestamp >= ymd_hms("2022-06-09 09:45:00"))
 
 
 # write it
 malletts %>%
   write_csv("plotdata/malletts.csv")
 
# Snow ----------------------------------------
this_winter <- Sys.Date() %>%
  tibble(today = .) %>%
  mutate(year = today %>% year(),
         month = today %>% month(),
         winter = case_when(
           month >= 9 ~ paste0(year, "-", (year+1)),
           month < 9 ~ paste0((year - 1), "-", year))) %>%
  pluck("winter", 1)

snow <- "https://waw.w3.uvm.edu/skivt-l/stinkfoot.php" %>%
  read_csv() %>%
  rename(id = ID,
         date = Date,
         precip = Precip,
         max_temp_degF = MaxT,
         min_temp_degF = MinT,
         new = New,
         depth_in = Depth) %>%
  mutate(year = date %>% year(),
         month = date %>% month(),
         week = date %>% week(),
         fake_year = case_when(
           month >= 9 ~ date %>% update(year = 1990),
           month < 9 ~ date %>% update(year = 1991)),
         julian_day = date %>% yday(),
         winter_day = julian_day - 250,
         winter = case_when(
           month >= 9 ~ paste0(year, "-", (year + 1)),
           month < 9 ~ paste0((year - 1), "-", year))) %>%
  filter(month >= 9 | month < 7) %>%
  drop_na(depth_in) %>%
  group_by(fake_year) %>%
  nest() %>%
  mutate(new = map(.x = data, .f = ~.x %>% #find percentile of each depth based on date
                     mutate(daily_percent_rank = depth_in %>% percent_rank()))) %>%
  select(-data) %>% 
  unnest(new) %>%
  mutate(percentile_label = (daily_percent_rank * 100) %>%
           round(digits = 1) %>%
           paste0("Percentile: ", .))

this_year <- snow %>%
  filter(winter == this_winter)

latest_date <- snow %>%
  pull("date") %>%
  max()

latest_depth <- snow %>%
  filter(date == latest_date) %>%
  pluck("depth_in", 1)

latest_percentile <- snow %>%
  filter(date == latest_date) %>%
  pluck("daily_percent_rank", 1) %>%
  prod(100) %>% # multiply by 100 to get %
  round(digits = 1)

 # write it
 snow %>%
  write_csv("plotdata/snow.csv")
 
 # Imagery -------------------------------
# link making function
  image_link <- function(date){
   year <- year(date)
   month <- month(date)
   day <- day(date)
   yday <- yday(date)
   year_last2 <- year %>%
     as.character() %>%
     str_trunc(2, "left", ellipsis = "")
   
   if (month < 10){month <- paste0("0", month)} # add a leading zero if less than 100
   
   if (day < 10){day <- paste0("0", day)} # add a leading zero if less than 10
   
   if (yday < 10){
     yday <- paste0("00", yday) # add two leading zeros if less than 10
   } else if (yday < 100){
     yday <- paste0("0", yday)} # add a leading zero if less than 100
   
   link <- 
     "https://ge.ssec.wisc.edu/modis-today/images/terra/true_color/" %>%
     paste0(year,
            "_",
            month,
            "_",
            day,
            "_",
            yday,
            "/t1.",
            year_last2,
            yday,
            ".USA4.143.250m.jpg"
     )
   
   return(link)
 }

 # create image for yesterday
   (Sys.Date() - 1) %>%
   image_link() %>%
   image_read() %>% # read in image from url
   image_crop("900x1200+2250+2150") %>%
   image_annotate(Sys.Date() - 1, 
                  size = 70, 
                  gravity = "SouthEast",
                  boxcolor = "grey85",
                  color = "black") %>%
   image_write("plotdata/sat_yesterday.png", format = "png")

 # function to make gif for past week
 gif_maker2 <- function(days_before){ # same as before, but just enter how many days you want to go back before yesterday
   yesterday <- Sys.Date() - 1
   
   date_list <- 
     seq((yesterday - days_before), yesterday, 1) %>%
     as.character() # to character
   
   gif <- date_list %>%
     tibble(dates = .) %>%
     mutate(links = map_chr(.x = dates, .f = ~.x %>% # create links using function above
                              image_link())) %>%
     pull("links") %>%
     image_read() %>% # read in image from url
     image_crop("900x1200+2250+2150") %>% # crop to Lake Champlain
     image_annotate(date_list, 
                    size = 70, 
                    gravity = "SouthEast",
                    boxcolor = "grey85",
                    color = "black") %>%
     image_animate(optimize = TRUE, # create animation from images
                   fps = 0.5)
   
   return(gif)
 }
 
 # write it
 gif_maker2(7) %>%
   image_write("plotdata/sat_week.gif",
               format = "gif")
 