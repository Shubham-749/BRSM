library(tidyverse)
library(stringr)

# --------------------------------------------------
# Load combined dataset
# --------------------------------------------------

data <- read.csv(
  "/Users/rohit.rohon01gmail.com/Downloads/combined_recognition_data.csv",
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# --------------------------------------------------
# Remove PsychoPy brackets and quotes
# --------------------------------------------------

clean_psychopy <- function(x){
  
  x <- gsub("\\[|\\]", "", x)   # remove []
  x <- gsub("'", "", x)         # remove quotes
  x <- gsub("None", NA, x)      # convert None to NA
  
  return(x)
}

data <- data %>%
  mutate(across(everything(), clean_psychopy))

# --------------------------------------------------
# Convert numeric columns
# --------------------------------------------------

numeric_cols <- c(
  "thisN","thisTrialN","thisRepN","movie_id",
  "resp.corr","resp.rt","resp.duration",
  "conf_radio.response","conf_radio.rt",
  "recogloop.resp.corr","recogloop.resp.rt",
  "recogloop.resp.duration",
  "confidence","confidence_rt"
)

data[numeric_cols] <- lapply(data[numeric_cols], as.numeric)

# --------------------------------------------------
# Clean whitespace
# --------------------------------------------------

data <- data %>%
  mutate(across(where(is.character), trimws))

# --------------------------------------------------
# Ensure factors
# --------------------------------------------------

data$Condition <- factor(data$`AB/NB`)
data$participant <- factor(data$Participant)

# --------------------------------------------------
# Fix frame column
# --------------------------------------------------

data$Frames <- factor(data$Frames)

# --------------------------------------------------
# Remove rows with missing accuracy
# --------------------------------------------------

data <- data %>%
  filter(!is.na(resp.corr))

# --------------------------------------------------
# Quick dataset checks
# --------------------------------------------------

cat("Number of rows:", nrow(data), "\n")
cat("Number of participants:", length(unique(data$Subject_id)), "\n")

table(data$Condition)
table(data$Frames)

# --------------------------------------------------
# Save cleaned dataset
# --------------------------------------------------

write.csv(
  data,
  "/Users/rohit.rohon01gmail.com/Downloads/final_cleaned_recognition_data.csv",
  row.names = FALSE
)

cat("Clean dataset saved successfully\n")