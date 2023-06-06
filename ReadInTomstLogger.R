###########################
### READ IN DATA ###
###########################
#
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("dataDownloader")
#install.packages("purrrlyr")
#install.packages("readxl")
# install.packages("fs")
# install.packages("purrr")
# install.packages("readr")
# install.packages("ggplot2")
#$source("https://raw.githubusercontent.com/audhalbritter/Three-D/master/R/Climate/soilmoisture_correction.R")
library(tidyverse)
library(lubridate)
# library(dataDownloader)
library(purrrlyr)
library(readxl)
library(fs)
library(purrr)
library(readr)
library(ggplot2)
# only needed for soiltemp template
# source("R/Rgathering/ReadInPlotLevel.R")

# get_file(node = "f4v9t",
#          file = "climate_tomst.zip",
#          path = "data",
#          remote_path = "Environment/raw_data/TOMST_microclimate_loggers/Lygra_southern_coastal"))
#get_file(node = "f4v9t",
 #        file = "LYGRA_2023.06.01.zip",
  #       path = "data",
#         remote_path = "Environment/raw_data/TOMST_microclimate_loggers/Lygra_southern_coastal")
#get_file(node = "f4v9t",
#         file = "INCLINE_metadata_LoggerDates.csv",
#         path = "data",
#        remote_path = "Environment/raw_data/TOMST_microclimate_loggers/Lygra_southern_coastal"))

#unzip("Environment/raw_data/TOMST_microclimate_loggers/Lygra_southern_coastal", exdir = "data")

#### CLIMATE DATA ####

# Read in meta data
metatomst <- read_excel(path="D:/Cours/DURIN/Code/DURIN_data/test_loggers.xlsx", col_names=TRUE, col_types=c("text","text","numeric","numeric","text","numeric","numeric","date","date"))
  # mutate(
  #   date_in = dmy(date_in), #dates in correct format
  #   date_out = dmy(date_out),
  # ) %>%

metatomst$date_in <- format(metatomst$date_in, "%d/%m/%Y")
metatomst$date_out <- format(metatomst$date_out, "%d/%m/%Y")
metatomst$loggerID<- as.factor(metatomst$loggerID)


### Read in files
files <- dir(path = "data/lygra", pattern = "^data.*\\.csv$", full.names = TRUE, recursive = TRUE)



# remove empty file
# files <- files[!(files %in% c(
# ))]

#files <- files[!(files %in% c(
#  "data/INCLINE_microclimate/data_94194607_2.csv",
#  "data/INCLINE_microclimate/data_94194699_3.csv"


# Function to read in data
temp_raw <- map_df(set_names(files), function(file) {
  file %>%
    set_names() %>%
    map_dfr(~ read_csv2(file = file,
                        col_names = FALSE,
                        # specify column types as not all .csv's match
                        # some examples of doubles and integers in X4
                        col_types = "n?nnnnnn"))
}, .id = "file")



# import cutting file fro cleaning

# cutting <- read_csv("data/INCLINE_microclimate_cut.csv", na = "", col_types = "ffTT")
# Create micrcolimate file merging TOMST and metadata files
microclimate <- temp_raw %>%
  # rename column names
  rename("ID" = "file", "datetime" = "X2", "time_zone" = "X3", "soil_temperature" = "X4", "ground_temperature" = "X5", "air_temperature" = "X6", "RawSoilmoisture" = "X7", "Shake" = "X8", "ErrorFlag" = "X9") %>%
  # change the date and time format
  mutate(datetime = ymd_hm(datetime)) %>%

  # Soil moisture calibration
  #mutate(SoilMoisture = a * RawSoilmoisture^2 + b * RawSoilmoisture + c) %>%

  # get logger ID by trimming characters from the file name
  mutate(
    loggerID = substr(ID, nchar(ID)-24, nchar(ID)-17),
    loggerID = as.factor(loggerID)
  ) %>%
  select(!c(ID, X1,X10)) %>% # remove unnessary columns
  distinct() %>%  # only choose unique records
  left_join(metatomst, by = "loggerID",relationship="many-to-many") %>%
  # group_by(loggerID) %>%
  # mutate(
  #   date_out = replace_na(date_out, today("CET")) #the logger still in the field don't have a date_out (NA in the metaData), but we need a date_out to filter. Today's date can only be correct because if you are never running this script on future data ;-)
  # ) %>%
  ## DURIN, do we need this?
  # filter(
  #   datetime > date_logger_in + 1
  #   & datetime < date_logger_out
  # )%>%

  ######## SOIL MOISTURE TEMPLATE CALCULATION ############

  # mutate( # calculate soil moisture
  #   soil_moisture = soil.moist(
  #     rawsoilmoist = RawSoilmoisture,
  #     soil_temp = soil_temperature,
  #     soilclass = "silt_loam" #it is the closest soil class, but still very wrong. The TMS calibration tool does not have any class for our soil
  #   )) %>%
  # select(!c(RawSoilmoisture, loggerID)) %>% # we want vertical tidy data
############################################################3
  pivot_longer(cols = c(air_temperature, soil_temperature, ground_temperature), names_to = "sensor", values_to = "value")



gc()


# cleaning ----------------------------------------------------------------



# graphs ------------------------------------------------------------------


#soil temperature
for (i in 1:9) {
  for (j in 1:3) {
    plot_number <- i + j/10
    file_name <- paste("loggers_soil_temperature_", as.character(i), "_", as.character(j), ".png", sep = "")
    filtered_data <- microclimate %>%
      filter(
        sensor == "soil_temperature" &
          datetime > "2023-04-22" &
          plotID == plot_number
      )

    plot <- ggplot(filtered_data, aes(x = datetime, y = value, color = "cutting")) +
      geom_line(aes(group = loggerID)) +
      scale_color_manual(values = c(
        "keep" = "#1e90ff",
        "cut" = "#ff0800"
      )) +
      scale_x_datetime(date_breaks = "1 month", minor_breaks = "10 day", date_labels = "%e/%m/%y") +
      facet_wrap(vars(loggerID), ncol = 3, scales = "free")

    ggsave(file.path("data/plots", file_name), plot, height = 40, width = 80, units = "cm")
  }
}
#ground_temperature
for (i in 1:9) {
  for (j in 1:3) {
    plot_number <- i + j/10
    file_name <- paste("loggers_ground_temperature_", as.character(i), "_", as.character(j), ".png", sep = "")

    filtered_data <- microclimate %>%
      filter(
        sensor == "ground_temperature" &
          datetime > "2023-04-22" &
          plotID == plot_number
      )

    plot <- ggplot(filtered_data, aes(x = datetime, y = value, color = "cutting")) +
      geom_line(aes(group = loggerID)) +
      scale_color_manual(values = c(
        "keep" = "#1e90ff",
        "cut" = "#ff0800"
      )) +
      scale_x_datetime(date_breaks = "1 month", minor_breaks = "10 day", date_labels = "%e/%m/%y") +
      facet_wrap(vars(loggerID), ncol = 3, scales = "free")

    ggsave(file.path("data/plots", file_name), plot, height = 40, width = 80, units = "cm")
  }
}

#air_temperature
for (i in 1:9) {
  for (j in 1:3) {
    plot_number <- i + j/10
    file_name <- paste("loggers_air_temperature_", as.character(i), "_", as.character(j), ".png", sep = "")

    filtered_data <- microclimate %>%
      filter(
        sensor == "air_temperature" &
          datetime > "2023-04-22" &
          plotID == plot_number
      )

    plot <- ggplot(filtered_data, aes(x = datetime, y = value, color = "cutting")) +
      geom_line(aes(group = loggerID)) +
      scale_color_manual(values = c(
        "keep" = "#1e90ff",
        "cut" = "#ff0800"
      )) +
      scale_x_datetime(date_breaks = "1 month", minor_breaks = "10 day", date_labels = "%e/%m/%y") +
      facet_wrap(vars(loggerID), ncol = 3, scales = "free")

    ggsave(file.path("data/plots", file_name), plot, height = 40, width = 80, units = "cm")
  }
}


# microclimate %>%
#   select(site) %>%
#   distinct()


# make clean dataset ------------------------------------------------------

microclimate_clean <- microclimate %>%
  filter(
    cutting == "keep"
  ) %>%
  select(datetime, loggerID, plotID, site, block, plot, OTC, treatment, comment, sensor, value)

gc()
# Save clean file
write_csv(microclimate_clean, "data_cleaned/INCLINE_microclimate.csv")




# #############
# Create micrcolimate file merging TOMST and metadata files
