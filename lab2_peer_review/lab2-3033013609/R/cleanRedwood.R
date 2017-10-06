# A function for cleaning the data
# be sure to load the packages from lab1.Rnw first!

cleanDatesData <- function(date_df) {
  # Arguments:
  #   date_df: a data.frame in the format of the output of the 
  #     loadDatesData() function
  # Returns:
  #   a data.frame similar to the input `dates` but with cleaned variables
  #     (number, day, date, time, datetime)
  
  # convert the dates variable to lubridate format
  date_df <- date_df %>% 
    # separate date variable into two variables: time and date
           # remove times
    mutate(date_sep = gsub("\\w+:\\w+:\\w+ ", "", date), 
           # remove day of the week
           date_sep = gsub("(Mon|Tue|Wed|Thu|Fri|Sat|Sun)", "", date_sep),
           # extract times
           time_sep = str_extract(as.character(date), " \\w+:\\w+:\\w+"),
           # combine date and time into single datetime variable
           datetime = mdy_hms(paste(date_sep, time_sep)),
           # convert day to a number
           day = as.numeric(as.character(day))) %>%
    # remove original date vairable and rename date_sep and time_sep
    select(-date, date = date_sep, time = time_sep) %>%
    # change number to numeric instead of factor
    #as.numeric(levels(dates$number))[dates$number]
    mutate(number = as.numeric(levels(number))[number])
  return(date_df)
}

cleanMoteLocationData <- function(mote_df){
  # Arguments:
  #   mote_df: a data.frame in the format of the output of the 
  #     loadMoteLocationData() function
  # Returns:
  #   a data.frame similar to the input but with cleaned variables
  #     (ADD NAMES)
  
  #change variable names 
  mote_df = mote_df %>% rename(node_id = ID, 
                               height = Height, 
                               direction = Direc,
                               distance = Dist,
                               tree = Tree) %>% 
  # add indicator of east and west 
  mutate(east_west_south = 
            ifelse(direction %in% c('E', 'NE'),'East',
                  ifelse(direction %in% c('S','ESE'),'South','West')))
  return(mote_df)
}

cleanRedwoodData <- function(redwood_df) {
  # Arguments:
  #   redwood_df: a data.frame in the format of the output of the 
  #     loadRedwoodData() function
  # Returns:
  #   a data.frame similar to the input but with cleaned variables
  #     (ADD NAMES)
  
  # convert result_time to lubridate ymd_hms format
  redwood_df <- redwood_df %>% 
    # change to lubridate format and save in result_time
    mutate(result_time = result_time %>% 
            ymd_hms()) %>% 
    # convert to central time 
    mutate(result_time = result_time - hours(7)) %>%
    # change variable names 
    rename(node_id = nodeid,
           number = epoch,
           temp = humid_temp, 
           incident_PAR = hamatop, 
           reflected_PAR = hamabot) %>%
    # delete missing value
    na.omit() %>%
    # delete gross outliers in temperature 
    filter(temp>0 & temp <35) %>%
    # delete node 145 after epoch 2500, because begins faulty temp readings 
    filter(!(node_id == 145 & number >2500)) %>%
    # delete node 78 after epoch 2910, because begins faulty temp readings 
    filter(!(node_id == 78 & number >2910)) %>%
    # delete node 3 after epoch 3490, because begins faulty temp readings 
    filter(!(node_id == 3 & number >3490)) %>%
    # delete node 123 after epoch 4625, because begins faulty temp readings 
    filter(!(node_id == 123 & number >4625)) %>%
    # delete node 59 after epoch 5840, because begins faulty temp readings 
    filter(!(node_id == 59 & number >5840)) %>%
    # delete node 59 after epoch 5840, because begins faulty temp readings 
    filter(!(node_id == 141 & number >9000)) %>%
    # delete node 64 after epoch 2470, because begins faulty temp readings 
    filter(!(node_id == 64 & number >2470)) %>%
    # delete node 2 after epoch 50, because begins faulty temp readings 
    filter(!(node_id == 2 & number >50)) %>%
    # ignore first 500 epoch because the trees do not track together
    # this never happens again, and incident_PAR not reading
    filter(number>500) %>% 
    
    # beginning humidity cleaning 
    # delete node 122 after epoch 2000, because begins faulty humidity readings 
    filter(!(node_id == 122 & number >2000)) %>%
    # delete node 122 after epoch 2000, because begins faulty humidity readings 
    filter(!(node_id == 196 & number >3850)) %>%
    # delete node 118 between epoch 8775 and 8850, because begins faulty humidity readings 
    filter(!(node_id == 118 & number >8775 & number <8850)) %>%
    # remove outliers in humidity (humidity > 110) 
    filter(humidity < 110) %>% 
    # remove outlier (humidity > 65, temp > 22.2)
    filter(!(humidity > 65 & temp > 22.2)) %>%
  
    # beginning incident PAR cleaning
    # delete node 135, because begins faulty incident_PAR readings 
    filter(node_id != 135) %>%
  
    # transform voltage for net using values found in explore.Rmd
    mutate(voltage = ifelse(
      voltage > 100, (442.97 - voltage)/82.55, voltage))
    
    return(redwood_df)
}

combineWithDate <- function(data1, data2, join_by){
  # Arguments:
  #   data1: a data.frame to be joined with
  #   data2: a data.frame to be joined to data1
  #   join_by: a variable data1 and data2 have to join by 
  #     with all rows from data1 and all columns from data1 and data2
  # Returns:
  #   a data.frame combining data1 and data2 by
  
  # combine by node_id, copying location values to each observation
  combined <- data1 %>% left_join(data2, by = join_by)
  
  # omit na values 
  combined <- combined %>% na.omit()
  
  # return combined data.frame
  return(combined)
}

combineWithLocation <- function(data1, data2, join_by){
  # Arguments:
  #   data1: a data.frame to be joined with
  #   data2: a data.frame to be joined to data1
  #   join_by: a variable data1 and data2 have to join by 
  #     with all rows from data1 and all columns from data1 and data2
  # Returns:
  #   a data.frame combining data1 and data2 by
  
  # combine by node_id, copying location values to each observation
  combined <- data1 %>% left_join(data2, by = join_by) %>%
  
  # node 100 missing from date so delete those 
  filter(node_id != 100) %>%
  
  # omit na values 
  na.omit()


  # return combined data.frame
  return(combined)
}

cleanRedwoodDataWithBegin <- function(redwood_df) {
  # Arguments:
  #   redwood_df: a data.frame in the format of the output of the 
  #     loadRedwoodData() function
  # Returns:
  #   a data.frame similar to the input but with cleaned variables
  #     (ADD NAMES)
  
  # convert result_time to lubridate ymd_hms format
  redwood_df <- redwood_df %>% 
    # change to lubridate format and save in result_time
    mutate(result_time = result_time %>% 
             ymd_hms()) %>% 
    # convert to central time 
    mutate(result_time = result_time - hours(7)) %>%
    # change variable names 
    rename(node_id = nodeid,
           number = epoch,
           temp = humid_temp, 
           incident_PAR = hamatop, 
           reflected_PAR = hamabot) %>%
    # delete missing value
    na.omit() %>%
    # delete gross outliers in temperature 
    filter(temp>0 & temp <35) %>%
    # delete node 145 after epoch 2500, because begins faulty temp readings 
    filter(!(node_id == 145 & number >2500)) %>%
    # delete node 78 after epoch 2910, because begins faulty temp readings 
    filter(!(node_id == 78 & number >2910)) %>%
    # delete node 3 after epoch 3490, because begins faulty temp readings 
    filter(!(node_id == 3 & number >3490)) %>%
    # delete node 123 after epoch 4625, because begins faulty temp readings 
    filter(!(node_id == 123 & number >4625)) %>%
    # delete node 59 after epoch 5840, because begins faulty temp readings 
    filter(!(node_id == 59 & number >5840)) %>%
    # delete node 59 after epoch 5840, because begins faulty temp readings 
    filter(!(node_id == 141 & number >9000)) %>%
    # delete node 64 after epoch 2470, because begins faulty temp readings 
    filter(!(node_id == 64 & number >2470)) %>%
    # delete node 2 after epoch 50, because begins faulty temp readings 
    filter(!(node_id == 2 & number >50)) %>%
    
    # beginning humidity cleaning 
    # delete node 122 after epoch 2000, because begins faulty humidity readings 
    filter(!(node_id == 122 & number >2000)) %>%
    # delete node 122 after epoch 2000, because begins faulty humidity readings 
    filter(!(node_id == 196 & number >3850)) %>%
    # delete node 118 between epoch 8775 and 8850, because begins faulty humidity readings 
    filter(!(node_id == 118 & number >8775 & number <8850)) %>%
    # remove outliers in humidity (humidity > 110) 
    filter(humidity < 110) %>% 
    # remove outlier (humidity > 65, temp > 22.2)
    filter(!(humidity > 65 & temp > 22.2)) %>%
    
    # beginning incident PAR cleaning
    # delete node 135, because begins faulty incident_PAR readings 
    filter(node_id != 135) %>%
    
    # transform voltage for net using values found in explore.Rmd
    mutate(voltage = ifelse(
      voltage > 100, (442.97 - voltage)/82.55, voltage))
  
  return(redwood_df)
}

combineWithDate <- function(data1, data2, join_by){
  # Arguments:
  #   data1: a data.frame to be joined with
  #   data2: a data.frame to be joined to data1
  #   join_by: a variable data1 and data2 have to join by 
  #     with all rows from data1 and all columns from data1 and data2
  # Returns:
  #   a data.frame combining data1 and data2 by
  
  # combine by node_id, copying location values to each observation
  combined <- data1 %>% left_join(data2, by = join_by)
  
  # omit na values 
  combined <- combined %>% na.omit()
  
  # return combined data.frame
  return(combined)
}
