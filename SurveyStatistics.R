SurveyStatistics<-function(data, cols_to_analyze) {
  
  #create new dataframe where i will put results. 
  
  results_df <- data.frame( 
    Column_Name = character(0),
    Mean_Yes = double(0),
    Mean_No = double(0),
    Mean_Difference = double(0),
    P_Value = double(0)
  )
  
  # Loop through the columns to analyze
  for (col_name in cols_to_analyze) {
    # Calculate the means for the current column, grouped by 'ins'
    means <- data %>%
      group_by(ins) %>% #group by insured or not
      summarize(mean_value = mean(get(col_name), na.rm = TRUE)) #calculates the mean for a certain column 
    #and group of aggregation, NAs are removed
    
    #Summarize returns a tibble data type: A tibble, or tbl_df, is a modern reimagining of the data.frame, 
    #keeping what time has proven to be effective, and throwing out what is not. 
    #Tibbles are data.frames that are lazy and surly: they do less (i.e. they don’t change variable names or types, 
    #and don’t do partial matching) and complain more (e.g. when a variable does not exist). 
    #This forces you to confront problems earlier, typically leading to cleaner, more expressive code
    #Subsetting: Subsetting a tibble with [ or subset() returns another tibble, preserving the tibble structure. 
    #This can be different from dataframes, which might return vectors or matrices in certain cases.
    
    # Calculate the difference between the means of 'Yes' and 'No' groups. Just creates a new column in means
    # and then it saves everything in mean_difference.     
    mean_difference <- means %>% 
      mutate(diff_value = mean_value[ins == 'Yes'] - mean_value[ins == 'No'])
    
    # Perform t-test for the current column on the 'Yes' and 'No' groups, excluding NAs-
    #First it selects the data for insured units, for col_name values that are not missing, 
    #then it does the same wrt to not insured units, then it performs a two sided t-test.
    #T.test() returns a list with all the parameters etc
    ttest_result <- t.test(data[data$ins == 'Yes' & !is.na(data[[col_name]]), col_name],
                           data[data$ins == 'No' & !is.na(data[[col_name]]), col_name])
    
    
    #rbind merges two dataframes along rows. the dataframe merged to results_df is here created with all the previous
    #results
    results_df <- rbind(results_df, data.frame(
      Column_Name = col_name,
      Mean_Yes = means$mean_value[means$ins == 'Yes'],
      Mean_No = means$mean_value[means$ins == 'No'],
      Mean_Difference = means$mean_value[means$ins == 'Yes'] - means$mean_value[means$ins == 'No'],
      P_Value = ttest_result$p.value
    ))
  }
  
  return(results_df)
}
