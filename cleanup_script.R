library(tidyverse)
library(stringr)

data <- read.csv("/Users/rohit.rohon01gmail.com/Downloads/data.csv", stringsAsFactors = FALSE)

clean_psychopy <- function(x) {
  
  x <- gsub("\\[|\\]", "", x)     # remove brackets
  x <- gsub("'", "", x)           # remove quotes
  x <- gsub("None", NA, x)        # convert None to NA
  
  return(x)
}

data <- data %>%
  mutate(across(everything(), clean_psychopy))

numeric_cols <- c(
  "thisN","thisTrialN","thisRepN","movie_id",
  "resp.corr","resp.rt","resp.duration",
  "conf_radio.response","conf_radio.rt",
  "recogloop.resp.corr","recogloop.resp.rt",
  "confidence","confidence_rt"
)

data[numeric_cols] <- lapply(data[numeric_cols], as.numeric)

data$FrameType <- str_extract(data$target_img, "(?<=_)BB|EM")

data$VideoID <- str_extract(data$target_img, "(?<=Vid)\\d+")
data$VideoID <- as.numeric(data$VideoID)

glimpse(data)
summary(data)

data <- data %>%
  filter(!is.na(resp.rt))

write.csv(data, "/Users/rohit.rohon01gmail.com/Downloads/cleaned_recognition_data.csv", row.names = FALSE)