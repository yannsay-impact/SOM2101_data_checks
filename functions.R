old_new <- function(data_raw, 
                    data_clean, 
                    cleaning_log,
                    variable,
                    uuid_raw,
                    uuid_clean,
                    uuid_cleaning_log) {
  if(cleaning_log[[variable]] %in% names(data_raw) & cleaning_log[[variable]] %in% names(data_clean) & 
     cleaning_log[[uuid_cleaning_log]] %in% data_clean[[uuid_clean]]){
    row_raw <- which(data_raw[[uuid_raw]] == cleaning_log[[uuid_cleaning_log]])
    value_raw <- data_raw[row_raw, cleaning_log[[variable]]]
    row_clean <- which(data_clean[[uuid_clean]] == cleaning_log[[uuid_cleaning_log]])
    value_clean <- data_clean[row_clean, cleaning_log[[variable]]]
    return_value <- data.frame(value_raw,
                               value_clean, 
                               binding = paste0( cleaning_log[[uuid_cleaning_log]], cleaning_log[[variable]]))
    names(return_value) <- c("value_raw", "value_clean", "binding")
  } else {
    return_value <- data.frame(value_raw = NULL,
                               value_clean = NULL,
                               binding = NULL)
  }
  
  return(return_value)     
  
}