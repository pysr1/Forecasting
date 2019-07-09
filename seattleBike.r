library(tidyverse)
library(lubridate)
source('module.r')
pages = c(
  'Fremont (SB) bicycle count',
  '2nd Ave Cycle Track North of Marion St',
  'Burke Gilman Trail north of NE 70th St Bike and Ped Counter',
  '39th Ave NE Greenway at NE 62nd St',
  'Broadway Cycle Track North Of E Union St',
  'NW 58th St Greenway at 22nd Ave NW Bike Counter',
  'Elliott Bay Trail in Myrtle Edwards Park',
  '26th Ave SW Greenway at SW Oregon St',
  'MTS Trail west of I-90 Bridge',
  'Chief Sealth Trail North of Thistle',
  'Spokane St Bridge Counter',
  'Westlake PBL and Newton St',
  'Second Ave Cycle Track South of Cedar St'
  
)

urls = c(
  'https://data.seattle.gov/api/views/aggm-esc4/rows.csv?accessType=DOWNLOAD',
  'https://data.seattle.gov/api/views/avwm-i8ym/rows.csv?accessType=DOWNLOAD',
  'https://data.seattle.gov/api/views/2z5v-ecg8/rows.csv?accessType=DOWNLOAD',
  'https://data.seattle.gov/api/views/3h7e-f49s/rows.csv?accessType=DOWNLOAD',
  'https://data.seattle.gov/api/views/j4vh-b42a/rows.csv?accessType=DOWNLOAD',
  'https://data.seattle.gov/api/views/47yq-6ugv/rows.csv?accessType=DOWNLOAD',
  'https://data.seattle.gov/api/views/4qej-qvrz/rows.csv?accessType=DOWNLOAD',
  'https://data.seattle.gov/api/views/mefu-7eau/rows.csv?accessType=DOWNLOAD',
  'https://data.seattle.gov/api/views/u38e-ybnc/rows.csv?accessType=DOWNLOAD',
  'https://data.seattle.gov/api/views/uh8h-bme7/rows.csv?accessType=DOWNLOAD',
  'https://data.seattle.gov/api/views/upms-nr8w/rows.csv?accessType=DOWNLOAD',
  'https://data.seattle.gov/api/views/675b-cqew/rows.csv?accessType=DOWNLOAD',
  'https://data.seattle.gov/api/views/heuk-48p8/rows.csv?accessType=DOWNLOAD'
)


read_bikes = function(file, colname, freq = "weeks"){
  df = vroom::vroom(file) %>% gather(key, value, -Date) %>% mutate(Date = mdy_hms(Date)) %>% 
    group_by(Date = floor_date(Date, freq), key) %>% 
    summarise(value = sum(value, na.rm = T)) %>% 
    ungroup() %>% 
    arrange(Date, key) %>% 
    mutate(index = colname, First = word(index, 1), key = paste(key, First, sep = "_")) 
  return(df %>% select(-index, -First))
}

map2_df(urls, pages, read_bikes) %>%  write_rds("seatleBike.rds")
