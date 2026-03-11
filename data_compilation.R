library(tidyverse)
library(stringr)

# -----------------------------
# Folder containing CSV files
# -----------------------------

folder_path <- "/Users/rohit.rohon01gmail.com/Downloads/BRSM data csv/BRSM data csv/"

file_list <- list.files(folder_path, pattern = "*.csv", full.names = TRUE)

combined_data <- list()

# -----------------------------
# Loop through all csv files
# -----------------------------

for(file in file_list){
  
  data <- read.csv(file, check.names = FALSE, stringsAsFactors = FALSE)
  
  # Skip files with very few rows
  if(nrow(data) < 10){
    cat("Skipping incomplete file:", file_name, "\n")
    next
  }
  
  # Clean column names
  names(data) <- trimws(names(data))
  
  # Remove empty column names
  data <- data[, names(data) != ""]
  
  # -----------------------------------
  # Extract Participant from filename
  # -----------------------------------
  
  file_name <- basename(file)
  
  participant <- str_extract(file_name, "^[^_]+_[^_]+")
  
  subject_id <- as.numeric(str_extract(participant, "\\d+"))
  
  condition <- str_extract(participant, "(?<=_)AB|NB")
  
  # -----------------------------------
  # Convert vigilance columns
  # -----------------------------------
  
  data$`Videos.stopped` <- as.numeric(data$`Videos.stopped`)
  data$`instruction_2.stopped` <- as.numeric(data$`instruction_2.stopped`)
  
  encoding_time <- (max(data$`Videos.stopped`, na.rm = TRUE) -
                      min(data$`instruction_2.stopped`, na.rm = TRUE)) / 60
  
  # -----------------------------------
  # Vigilance check
  # -----------------------------------
  
  if("Videos.stopped" %in% names(data) & "instruction_2.stopped" %in% names(data)){
    
    data$`Videos.stopped` <- as.numeric(data$`Videos.stopped`)
    data$`instruction_2.stopped` <- as.numeric(data$`instruction_2.stopped`)
    
    encoding_time <- (max(data$`Videos.stopped`, na.rm = TRUE) -
                        min(data$`instruction_2.stopped`, na.rm = TRUE)) / 60
    
    if(encoding_time > 27.05){
      
      cat("Removed due to vigilance:", subject_id,
          "Encoding time:", round(encoding_time,2), "minutes\n")
      
      next
    }
    
  } else {
    
    cat("Skipping vigilance check for:", subject_id,
        "- required columns missing\n")
    
  }
  
  # -----------------------------------
  # Filter rows where thisN 0–39
  # -----------------------------------
  
  data <- data %>%
    filter(thisN >= 0 & thisN <= 39)
  
  # -----------------------------------
  # Extract frame type (BB / EM)
  # -----------------------------------
  
  data$Frames <- str_extract(data$target_img, "(?<=_)BB|EM")
  
  # -----------------------------------
  # Select required columns
  # -----------------------------------
  
  data <- data %>%
    select(
      thisN,thisTrialN,thisRepN,movie_id,
      target_img,lure_img,
      resp.keys,resp.corr,resp.rt,resp.duration,
      conf_radio.response,conf_radio.rt,
      recognition_task.started,leftimage.started,rightimage.started,
      resp.started,frames_question.started,right_label.started,
      left_label.started,recognition_task.stopped,
      recogloop.resp.keys,recogloop.resp.corr,
      recogloop.resp.rt,recogloop.resp.duration,
      confidence.started,conf_text.started,
      conf_radio.started,conf_label.started,
      confidence.stopped,recogloop.conf_radio.response,
      recogloop.conf_radio.rt,confidence,confidence_rt
    )
  
  # -----------------------------------
  # Add Participant columns
  # -----------------------------------
  
  data <- data %>%
    mutate(
      Participant = participant,
      Subject_id = subject_id,
      `AB/NB` = condition,
      Frames = str_extract(target_img, "(?<=_)BB|EM")
    )
  
  # Move columns to front
  data <- data %>%
    select(Participant, Subject_id, `AB/NB`, everything())
  
  combined_data[[length(combined_data) + 1]] <- data
}

# -----------------------------
# Combine all datasets
# -----------------------------

final_dataset <- bind_rows(combined_data)

# -----------------------------
# Save combined dataset
# -----------------------------

write.csv(final_dataset,
          "/Users/rohit.rohon01gmail.com/Downloads/combined_recognition_data.csv",
          row.names = FALSE)

cat("Dataset created successfully\n")