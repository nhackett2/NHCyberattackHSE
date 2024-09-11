library(readxl)
pre_data <- read_excel("C:/Users/nhack/Downloads/2pre_attack_data_CUT.xlsx")

str(pre_data)

post_data <- read_excel("C:/Users/nhack/Downloads/post_attack_data_CUT.xlsx")

str(post_data)


# Now check for NA values:
  
any(is.na(pre_data))
any(is.na(post_data))

sum(is.na(pre_data))


#For the 20 of the NAs, it is the adult/child column and age profile column for Beaumont Hospital in the 
#range of 0-3 months - since figures are mostly 1 with a few 2s and 3s, can deem in significant and delete. 

pre_data <- na.omit(pre_data)
any(is.na(pre_data))

#Checked and no NAs left. Can begin grouping the ranges:

library(dplyr)
library(magrittr)
pre_data_grouped <- pre_data %>%
  mutate(Time_Band_Grouped = case_when(
    `Time Band` %in% c("0-3 Months", "3-6 Months") ~ "0-6 Months",
    `Time Band` %in% c("6-9 Months", "9-12 Months") ~ "6-12 Months",
    `Time Band` %in% c("12-15 Months", "15-18 Months") ~ "12-18 Months",
    `Time Band` == "18+ Months" ~ "18+ Months",
    TRUE ~ "18+ Months"
  ))

#To check if "18+ Months" is present, as named differently in the different files
print(unique(pre_data_grouped$Time_Band_Grouped))


#now taking away age profile and time band variables
pre_data_summarised <- pre_data_grouped %>%
  group_by(ArchiveDate, Hospital, `Adult/Child`, `Time_Band_Grouped`) %>%
  summarise(Total = sum(Total, na.rm = TRUE)) %>%
  ungroup()

#Now to get total sum which is the sum of all ranges per hospital per month:

pre_data_total <- pre_data_summarised %>%
  group_by(ArchiveDate, Hospital, `Adult/Child`) %>%
  mutate(Sum_Total = sum(Total, na.rm = TRUE)) %>%
  ungroup()

#converting ArchiveDate to date format:

pre_data_total$ArchiveDate <- as.Date(pre_data_total$ArchiveDate, format="%d/%m/%Y")
post_data$ArchiveDate <- as.Date(post_data$ArchiveDate, format="%d/%m/%Y")
  

#to format the same way and rename Sum_Total to Total to align. 

library(tidyr)
library(dplyr)

# Rename columns in the post_data dataset first
post_data_renamed <- post_data %>%
  rename(
    `18+ Months` =  `18 Months +`  
  )

# Converting from wide format to long format
post_data_long <- post_data_renamed %>%
  pivot_longer(
    cols = c(`0-6 Months`, `6-12 Months`, `12-18 Months`, `18+ Months`), 
    names_to = "Time_Band_Grouped",  
    values_to = "Total_per_age_profile"     
  ) %>%
  ungroup()

#reshaped data
head(post_data_long)


#Rename columns
post_data_long2 <- post_data_long %>%
  rename(
    Sum_Total = Total,              
    Total = Total_per_age_profile   
  )

#View
head(post_data_long2)


#Now making one combined data set for analysis

combined_data <- bind_rows(pre_data_total, post_data_long2, .id = "source")

#check if any zero values:
total_exact_zeros <- sum(combined_data == 0)
print(total_exact_zeros)

#So we do not have any zero values, i.e. no missing data or data entry errors.


#view the strings
str(pre_data_total)
str(post_data_long2)
str(combined_data)


#Methodology - Now moving onto EDA - visualisation by plots to understand the data more.


#HEATMAP
library(dplyr)
library(ggplot2)

#Aggregate the data so Sum_Total does not duplicate
pre_aggregated_data <- pre_data_total %>%
  group_by(Hospital, ArchiveDate, Time_Band_Grouped, Total) %>%
  summarise(Sum_Total = sum(Sum_Total, na.rm = TRUE), .groups = 'drop')

post_aggregated_data <- post_data_long2 %>%
  group_by(Hospital, ArchiveDate, Time_Band_Grouped, Total) %>%
  summarise(Sum_Total = sum(Sum_Total, na.rm = TRUE), .groups = 'drop')

combined_aggregated_data <- combined_data %>%
  group_by(Hospital, ArchiveDate, Time_Band_Grouped, Total) %>%
  summarise(Sum_Total = sum(Sum_Total, na.rm = TRUE), .groups = 'drop')


#range of Sum_Total for proper scaling
max_value <- max(pre_aggregated_data$Sum_Total, na.rm = TRUE)
min_value <- min(pre_aggregated_data$Sum_Total, na.rm = TRUE)

ggplot(pre_aggregated_data, aes(x = Hospital, y = ArchiveDate, fill = Sum_Total)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue", limits = c(min_value, max_value)) +
  labs(title = "Heatmap of Total Waiting Times by Hospital Over Time pre cyber attack",
       x = "Hospital",
       y = "Date",
       fill = "Total Waiting Time") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


max_value <- max(post_aggregated_data$Sum_Total, na.rm = TRUE)
min_value <- min(post_aggregated_data$Sum_Total, na.rm = TRUE)

ggplot(post_aggregated_data, aes(x = Hospital, y = ArchiveDate, fill = Sum_Total)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue", limits = c(min_value, max_value)) +
  labs(title = "Heatmap of Total Waiting Times by Hospital Over Time post cyber attack",
       x = "Hospital",
       y = "Date",
       fill = "Total Waiting Time") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

max_value <- max(combined_aggregated_data$Sum_Total, na.rm = TRUE)
min_value <- min(combined_aggregated_data$Sum_Total, na.rm = TRUE)


ggplot(combined_aggregated_data, aes(x = Hospital, y = ArchiveDate, fill = Sum_Total)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue", limits = c(min_value, max_value)) +
  labs(title = "Heatmap of Total Waiting Times by Hospital Over Time Overall",
       x = "Hospital",
       y = "Date",
       fill = "Total Waiting Time") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


#MULTIPANEL LATTICE PLOT - displays the relationship between Sum_Total and ArchiveDate across different 
#panels and grouped by the Adult/Child variable.

library(lattice)

xyplot(Sum_Total ~ ArchiveDate | Hospital, 
       data = combined_data, 
       groups = `Adult/Child`, 
       type = "o", 
       auto.key = list(space = "right"), 
       xlab = "Date", 
       ylab = "Total Waiting Time", 
       layout = c(2, 5),
       main = "Total Waiting Time Trend by Hospital and Age Group")


#STACKED BAR CHART

#creating desired order for the time bands
time_band_order <- c("0-6 Months", "6-12 Months", "12-18 Months", "18+ Months")

#Ensuring the Time_Band_Grouped is a factor with the specified levels
combined_aggregated_data$Time_Band_Grouped <- factor(
  combined_aggregated_data$Time_Band_Grouped,
  levels = time_band_order
)


#stacked bar chart with correctly ordered time bands
ggplot(combined_aggregated_data, aes(x = ArchiveDate, y = Total, fill = Time_Band_Grouped)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Hospital, nrow = 5, ncol = 2) +  # Arrange plots in 5 rows and 2 columns
  labs(title = "Stacked Bar Chart of Total Waiting Times by Time Band for Each Hospital",
       x = "Date",
       y = "Total Waiting Time",
       fill = "Time Band") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#TREEMAP

library(treemap)

treemap(combined_aggregated_data,
        index = c("Hospital", "Time_Band_Grouped"),
        vSize = "Sum_Total",
        vColor = "Sum_Total",
        type = "value",
        title = "Treemap of Waiting Times by Hospital and Time Band",
        palette = "Blues") 



#VIOLIN PLOT

aggregated_data <- combined_aggregated_data %>%
  group_by(Hospital, ArchiveDate) %>%
  summarise(Sum_Total = sum(Sum_Total, na.rm = TRUE), .groups = 'drop')


ggplot(aggregated_data, aes(x = Hospital, y = Sum_Total)) +
  geom_violin(aes(fill = Hospital), scale = "width", trim = FALSE) +
  stat_summary(fun = median, geom = "point", shape = 18, size = 3, color = "red") +
  labs(title = "Violin Plot of Total Waiting Times Across Different Hospitals",
       x = "Hospital",
       y = "Total Waiting Time",
       fill = "Hospital") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#BOX PLOT
aggregated_data <- combined_aggregated_data %>%
  group_by(Hospital, ArchiveDate) %>%
  summarise(Sum_Total = sum(Sum_Total, na.rm = TRUE), .groups = 'drop')

ggplot(aggregated_data, aes(x = Hospital, y = Sum_Total)) +
  geom_boxplot(aes(fill = Hospital), outlier.colour = "red", outlier.size = 2) +
  labs(title = "Box Plot of Total Waiting Times Across Different Hospitals",
       x = "Hospital",
       y = "Total Waiting Time",
       fill = "Hospital") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#Replace outliers which are data entry errors with average of the before and after values:
library(dplyr)

#dates and hospitals of interest
error_date <- "2021-04-29"
before_date <- "2021-03-25"
after_date <- "2021-05-13"
hospitals_of_interest <- c("Beaumont Hospital", "Cavan General Hospital", "Cork University Hospital")

#Function to replace erroneous value but retain all records
replace_error <- function(data, error_date, before_date, after_date, hospitals) {
  # Convert dates to Date type if necessary
  data <- data %>%
    mutate(ArchiveDate = as.Date(ArchiveDate))
  
  corrected_data <- data %>%
    group_by(Hospital) %>%
    mutate(Sum_Total = ifelse(Hospital %in% hospitals & ArchiveDate == as.Date(error_date),
                              {
                                # Calculate average of before and after values
                                before_value <- Sum_Total[ArchiveDate == as.Date(before_date)]
                                after_value <- Sum_Total[ArchiveDate == as.Date(after_date)]
                                if (!is.na(before_value) & !is.na(after_value)) {
                                  mean(c(before_value, after_value), na.rm = TRUE)
                                } else {
                                  Sum_Total  
                                }
                              },
                              Sum_Total)) %>%
    ungroup()
  
  return(corrected_data)
}

#Apply the function to replace erroneous values
aggregated_data_corrected <- replace_error(aggregated_data, error_date, before_date, after_date, hospitals_of_interest)


#MODELLING

#TIME SERIES PLOT
library(scales)

aggregated_data_corrected <- aggregated_data_corrected %>%
  mutate(ArchiveDate = as.Date(ArchiveDate))

y_limits <- range(aggregated_data_corrected$Sum_Total, na.rm = TRUE)

#Defining the start and end of the period for interruption
start_date <- as.Date('2021-05-14')
end_date <- as.Date('2021-06-30')  


ggplot(data = aggregated_data_corrected) +
  geom_point(aes(x = ArchiveDate, y = Sum_Total)) +
  geom_line(aes(x = ArchiveDate, y = Sum_Total), group = 1) + 
  geom_rect(aes(xmin = start_date, xmax = end_date, ymin = -Inf, ymax = Inf),
            fill = 'red', alpha = 0.2) +  
  scale_y_continuous(limits = y_limits, labels = comma) + 
  xlab('Time') +
  ylab('Total') +
  labs(title = 'Time Series of the Outpatient Waiting List Numbers') +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.3, size = 5),
    axis.text = element_text(size = 5),
    axis.title = element_text(size = 5),
    plot.title = element_text(size = 10)
  ) +
  facet_wrap(~ Hospital, nrow = 5, scales = "free_y")  


#Applying OLS first to see autocorrelation levels with DW test:


#Define the start and end dates of the interruption
start_date <- as.Date('2021-05-14')
end_date <- as.Date('2021-06-30')

#PostIntervention variable for each hospital
aggregated_data_corrected <- aggregated_data_corrected %>%
  group_by(Hospital) %>%
  mutate(PostIntervention = case_when(
    ArchiveDate >= start_date & ArchiveDate <= end_date ~ 1, #at interuption
    ArchiveDate > end_date ~ 2,  #period after the interruption
    TRUE ~ 0  #period before the interruption
  )) %>%
  ungroup()


library(lmtest)

#empty list to store Durbin-Watson test results
dw_results <- list()

#Loop through each hospital
for (hospital in unique(aggregated_data_corrected$Hospital)) {
  

  hospital_data <- aggregated_data_corrected %>%
    filter(Hospital == hospital)
  
  #OLS model
  ols_model <- lm(Sum_Total ~ ArchiveDate * PostIntervention, data = hospital_data)
  
  #DW test for autocorrelation
  dw_test <- dwtest(ols_model)
  
  dw_results[[hospital]] <- dw_test
  
  print(paste("Durbin-Watson test result for hospital:", hospital))
  print(dw_test)
}


#evident high autocorrelation - before PW split into train and test:

aggregated_data_corrected$ArchiveDate <- as.Date(aggregated_data_corrected$ArchiveDate)

#split point
split_date <- as.Date("2023-01-26")

train_data <- aggregated_data_corrected %>% filter(ArchiveDate < split_date)
test_data <- aggregated_data_corrected %>% filter(ArchiveDate >= split_date)


#The data is split by 81% train data and 29% test data. The train data includes pre cyber-attack and 
#approximately a year and a half post cyber attack. The test data is approximately one year and a half.

#PW REGRESSION

library(prais)
library(broom)

model_summaries <- list()

#Loop through each hospital
for (hospital in unique(train_data$Hospital)) {
  
  hospital_data <- train_data %>%
    filter(Hospital == hospital) %>%
    arrange(ArchiveDate)  
  
  hospital_data$ArchiveDate <- as.numeric(as.Date(hospital_data$ArchiveDate))
  
  hospital_data$PostIntervention <- ifelse(hospital_data$ArchiveDate >= as.numeric(as.Date('2021-07-29')), 1, 0)
  
  #Fit PW
  pw_model <- tryCatch({
    prais::prais_winsten(Sum_Total ~ ArchiveDate * PostIntervention,
                         index = 'ArchiveDate',
                         data = hospital_data)
  }, error = function(e) {
    message(paste("Error for hospital:", hospital, ":", e$message))
    return(NULL)
  })
  

  if (!is.null(pw_model)) {
    # Store the summary of the model
    model_summaries[[hospital]] <- summary(pw_model)
    

    print(paste("Summary for hospital:", hospital))
    print(summary(pw_model))
  }
}


if ("Hospital_A" %in% names(model_summaries)) {
  print(model_summaries[["Hospital_A"]])
}



#ADJUSTED R^2
adjusted_r2_table <- data.frame(Hospital = character(), Adjusted_R2 = numeric(), stringsAsFactors = FALSE)


for (hospital in names(model_summaries)) {
  adj_r2 <- model_summaries[[hospital]]$adj.r.squared
  
  adjusted_r2_table <- rbind(adjusted_r2_table, data.frame(Hospital = hospital, Adjusted_R2 = adj_r2))
}

print(adjusted_r2_table)


#COUNTER FACTUAL AND FACTUAL TIME SERIES PLOT
library(ggplot2)
library(scales)
library(prais)


intervention_start_date <- as.Date('2021-07-29')

plots <- list()

#Loop through each hospital
for (hospital in unique(train_data$Hospital)) {
  
  # Filter data for the current hospital
  hospital_data <- train_data %>%
    filter(Hospital == hospital) %>%
    mutate(ArchiveDate_numeric = as.numeric(ArchiveDate)) 
  
  hospital_data <- hospital_data %>%
    mutate(PostIntervention = if_else(ArchiveDate >= intervention_start_date, 1, 0))
  

  pw_model <- tryCatch({
    prais_winsten(Sum_Total ~ ArchiveDate_numeric * PostIntervention,
                  index = 'ArchiveDate_numeric',
                  data = hospital_data)
  }, error = function(e) {
    message(paste("Error for hospital:", hospital, ":", e$message))
    next
  })
  

  if (!is.null(pw_model)) {
    #Calculate factual trend and counter-factual trend
    hospital_data <- hospital_data %>%
      mutate(factual_trend = pw_model$coefficients[1] +
               pw_model$coefficients[2] * ArchiveDate_numeric +
               pw_model$coefficients[3] * PostIntervention +
               pw_model$coefficients[4] * ArchiveDate_numeric * PostIntervention,
             counter_fact = pw_model$coefficients[1] +
               pw_model$coefficients[2] * ArchiveDate_numeric)
    
    p <- ggplot(data = hospital_data) +
      geom_point(aes(x = ArchiveDate, y = Sum_Total), color = 'black') +
      geom_line(aes(x = ArchiveDate, y = Sum_Total), color = 'black') +
      
      #Vertical line to separate before and after intervention
      geom_vline(xintercept = as.numeric(intervention_start_date), linetype = "dotdash", color = 'red', size = 1.2) +
      
      #Plot factual trend
      geom_line(aes(x = ArchiveDate, y = factual_trend), linetype = "dashed", color = 'blue', size = 1.2) +
      
      #Plot counter-factual trend
      geom_line(aes(x = ArchiveDate, y = counter_fact), linetype = "dashed", color = 'darkorange', size = 1.2) +
      
      scale_y_continuous(labels = comma) +
      xlab('Date') +
      ylab('Number on Waiting List') +
      labs(title = paste('Time Series of Waiting List for Hospital:', hospital)) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.3, size = 16),
            axis.text = element_text(size = 16),
            axis.title = element_text(size = 16),
            plot.title = element_text(size = 20))
    
   
    plots[[hospital]] <- p
    
    #Save the plot to a file
    ggsave(filename = paste0("plot_", hospital, ".png"), plot = p, width = 10, height = 6)
  }
}

#print plots to the console
for (hospital in names(plots)) {
  print(plots[[hospital]])
}


#RMSE


#compute RMSE
compute_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

#Function to get fitted values from a Prais-Winsten model
get_fitted_values <- function(model, newdata) {
  tryCatch({
    
    coefficients <- model$coefficients[, "Estimate"]
    
    
    model_matrix <- model.matrix(as.formula(model$call$formula), data = newdata)
    
    
    predictions <- model_matrix %*% coefficients
    
    return(predictions)
  }, error = function(e) {
    message(paste("Error in getting fitted values:", e$message))
    return(NULL)
  })
}


model_rmse_list <- list()

#Loop through each hospital and compute metrics
for (hospital in unique(test_data$Hospital)) {
  hospital_data <- test_data %>%
    filter(Hospital == hospital)
  
  model_for_hospital <- model_summaries[[hospital]]
  
  if (!is.null(model_for_hospital)) {
    predictions <- get_fitted_values(model_for_hospital, hospital_data)
    
    if (!is.null(predictions)) {
      model_rmse <- compute_rmse(hospital_data$Sum_Total, predictions)

      model_rmse_list[[hospital]] <- model_rmse
    } else {
      message(paste("No predictions available for hospital:", hospital))
    }
  } else {
    message(paste("No model available for hospital:", hospital))
  }
}

#Convert model RMSE list to a data frame
model_rmse_df <- data.frame(
  Hospital = names(model_rmse_list),
  Model_RMSE = unlist(model_rmse_list)
)

print("Model RMSE for test data:")
print(model_rmse_df)

mean_actual <- mean(test_data$Sum_Total, na.rm = TRUE)
rmse_ratio <- model_rmse_df$Model_RMSE / mean_actual
print("RMSE relative to mean actual values:")
print(rmse_ratio)


#Now plotting actual vs. predicted values to see how accurate the model was visually:


actual_values_df <- test_data %>%
  select(Hospital, Sum_Total, ArchiveDate) %>%
  rename(Actual = Sum_Total)

get_predictions_for_hospital <- function(model, data) {
  return(get_fitted_values(model, data)) 
}

predicted_values_list <- list()

for (hospital in unique(test_data$Hospital)) {
  hospital_data <- test_data %>%
    filter(Hospital == hospital)
  
  model_for_hospital <- model_summaries[[hospital]]
  
  if (!is.null(model_for_hospital)) {
   
    predictions <- get_predictions_for_hospital(model_for_hospital, hospital_data)
    

    predictions_df <- data.frame(
      Hospital = hospital,
      ArchiveDate = hospital_data$ArchiveDate,  
      Predicted = predictions
    )
    
    predicted_values_list[[hospital]] <- predictions_df
  } else {
    message(paste("No model available for hospital:", hospital))
  }
}

predicted_values_df <- do.call(rbind, predicted_values_list)

plot_data <- actual_values_df %>%
  left_join(predicted_values_df, by = c("Hospital", "ArchiveDate"))

print(head(plot_data))


library(ggplot2)

ggplot(plot_data, aes(x = ArchiveDate)) +
  geom_line(aes(y = Actual, color = "Actual"), size = 1) +  
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1, linetype = "dashed") +  
  facet_wrap(~ Hospital, nrow=5, ncol=2) + 
  labs(title = "Actual vs. Predicted Values Over Time by Hospital",
       x = "Archive Date",
       y = "Values",
       color = "Legend") +
  theme_minimal()
theme(
  strip.text = element_text(size = 6) 
)


#QQ PLOT AND RESIDUALS VS. FITTED
library(ggplot2)
library(dplyr)

compute_fitted_residuals <- function(model, newdata) {
  coefficients <- model$coefficients[, "Estimate"]
  
  model_matrix <- model.matrix(as.formula(model$call$formula), data = newdata)

  fitted_values <- model_matrix %*% coefficients
  

  residuals <- newdata$Sum_Total - fitted_values
  
  return(data.frame(Fitted = fitted_values, Residuals = residuals))
}


combined_diagnostic_data <- data.frame()

#Loop through each hospital to compute diagnostics
for (hospital in names(model_summaries)) {
  hospital_data <- test_data %>% filter(Hospital == hospital)

  pw_model_for_hospital <- model_summaries[[hospital]]
  
  if (!is.null(pw_model_for_hospital)) {
    #Compute fitted values and residuals
    diagnostic_data <- compute_fitted_residuals(pw_model_for_hospital, hospital_data)
    diagnostic_data$Hospital <- hospital  # Add hospital name for plotting
    combined_diagnostic_data <- rbind(combined_diagnostic_data, diagnostic_data)
  } else {
    message(paste("No Prais-Winsten model available for hospital:", hospital))
  }
}

# Plot residuals vs. fitted values with facets for each hospital
ggplot(combined_diagnostic_data, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  facet_wrap(~ Hospital, nrow = 5, ncol = 2) + 
  labs(title = "Residuals vs. Fitted Values by Hospital",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal() +
  theme(legend.position = "none",
        strip.text = element_text(size = 8)) 

# Q-Q Plot for residuals
ggplot(combined_diagnostic_data, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  facet_wrap(~ Hospital, nrow = 5, ncol = 2) + 
  labs(title = "Q-Q Plot of Residuals by Hospital",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal() +
  theme(strip.text = element_text(size = 8))  


#ARIMA MODEL 


#Before applying the model, the residuals must be stationary. A way to check this is 
#Augmented Dickey-Fuller (ADF) test.  First, we can plot the resduals to check for any noticeable pattern:
library(tseries) 
library(forecast) 

intervention_start_date <- as.Date('2021-07-29')

residuals_list <- list()
residual_plots <- list()
acf_pacf_plots <- list()
adf_results <- list()
differenced_acf_plots <- list()
second_order_differenced_acf_plots <- list()

#Loop over each hospital
for (hospital in unique(train_data$Hospital)) {
  
  
  hospital_data <- train_data %>%
    filter(Hospital == hospital) %>%
    mutate(ArchiveDate_numeric = as.numeric(ArchiveDate))  
  
 
  hospital_data <- hospital_data %>%
    mutate(PostIntervention = if_else(ArchiveDate >= intervention_start_date, 1, 0))
  
 
  pw_model <- tryCatch({
    prais_winsten(Sum_Total ~ ArchiveDate_numeric * PostIntervention,
                  index = 'ArchiveDate_numeric',
                  data = hospital_data)
  }, error = function(e) {
    message(paste("Error for hospital:", hospital, ":", e$message))
    next
  })
  
  
  if (!is.null(pw_model)) {
    
    residuals_pw <- residuals(pw_model)
    
    
    if (length(residuals_pw) == 0) {
      message(paste("No residuals for hospital:", hospital))
    } else {
      
      residuals_list[[hospital]] <- residuals_pw
      
      
      residuals_df <- data.frame(Date = hospital_data$ArchiveDate, Residuals = residuals_pw)
      
      #plot for residuals
      residuals_plot <- ggplot(data = residuals_df, aes(x = Date, y = Residuals)) +
        geom_line() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(title = paste('Residuals for Hospital:', hospital),
             x = 'Date',
             y = 'Residuals') +
        theme_minimal()
      
      
      residual_plots[[hospital]] <- residuals_plot
      
      #Perform ADF test
      adf_result <- adf.test(residuals_pw, alternative = "stationary")
      adf_results[[hospital]] <- adf_result
      
      #Create ACF and PACF plots
      acf_plot <- ggAcf(residuals_pw) +
        ggtitle(paste('ACF for Hospital:', hospital)) +
        theme_minimal()
      
      pacf_plot <- ggPacf(residuals_pw) +
        ggtitle(paste('PACF for Hospital:', hospital)) +
        theme_minimal()
      
    
      acf_pacf_plots[[hospital]] <- list(acf_plot = acf_plot, pacf_plot = pacf_plot)
      
     
      if (adf_result$p.value > 0.01) {
        #First-order differencing
        differenced_residuals <- residuals_pw - lag(residuals_pw, 1)
        differenced_residuals <- na.omit(differenced_residuals) 
        
        #Re-check ADF test on first-differenced residuals
        adf_test_differenced <- adf.test(differenced_residuals, alternative = "stationary")
        adf_results[[hospital]] <- adf_test_differenced
        
        #Create and save the ACF plot for first-differenced residuals
        differenced_acf_plot <- ggAcf(differenced_residuals) +
          ggtitle(paste('ACF of First-Differenced Residuals for Hospital:', hospital)) +
          theme_minimal()
        
       
        differenced_acf_plots[[hospital]] <- differenced_acf_plot
        
        #Second-order differencing if needed
        if (adf_test_differenced$p.value > 0.01) {
          second_order_differenced_residuals <- differenced_residuals - lag(differenced_residuals, 1)
          second_order_differenced_residuals <- na.omit(second_order_differenced_residuals)  # Remove NA values from differencing
          
          #Re-check ADF test on second-differenced residuals
          adf_test_second_order_differenced <- adf.test(second_order_differenced_residuals, alternative = "stationary")
          adf_results[[hospital]] <- adf_test_second_order_differenced
          
          #Create and save the ACF plot for second-order differenced residuals
          second_order_differenced_acf_plot <- ggAcf(second_order_differenced_residuals) +
            ggtitle(paste('ACF of Second-Order Differenced Residuals for Hospital:', hospital)) +
            theme_minimal()
          
        
          second_order_differenced_acf_plots[[hospital]] <- second_order_differenced_acf_plot
        }
      }
    }
  }
}


for (hospital in names(adf_results)) {
  print(paste("ADF Test Result for Hospital:", hospital))
  print(adf_results[[hospital]])
  
  if (!is.null(residual_plots[[hospital]])) {
    print(residual_plots[[hospital]])
  }
  if (!is.null(acf_pacf_plots[[hospital]])) {
    print(acf_pacf_plots[[hospital]]$acf_plot)
    print(acf_pacf_plots[[hospital]]$pacf_plot)
  }
  if (!is.null(differenced_acf_plots[[hospital]])) {
    print(differenced_acf_plots[[hospital]])
  }
  if (!is.null(second_order_differenced_acf_plots[[hospital]])) {
    print(second_order_differenced_acf_plots[[hospital]])
  }
}


#NOW FIT AUTO ARIMA MODEL:
library(dplyr)
library(forecast)
library(ggplot2)
library(tidyr)
library(lubridate)
library(purrr)


df_monthly_train <- train_data %>%
  group_by(Hospital, Month = floor_date(ArchiveDate, "month")) %>%
  summarise(Sum_Total = sum(Sum_Total, na.rm = TRUE), .groups = 'drop')


df_monthly_test <- test_data %>%
  group_by(Hospital, Month = floor_date(ArchiveDate, "month")) %>%
  summarise(Sum_Total = sum(Sum_Total, na.rm = TRUE), .groups = 'drop')


common_hospitals <- intersect(df_monthly_train$Hospital, df_monthly_test$Hospital)

df_monthly_train <- df_monthly_train %>% filter(Hospital %in% common_hospitals)
df_monthly_test <- df_monthly_test %>% filter(Hospital %in% common_hospitals)


df_monthly_ts_train <- df_monthly_train %>%
  group_by(Hospital) %>%
  nest() %>%
  mutate(
    ts_data = map(data, ~ts(.x$Sum_Total, frequency = 12, start = c(year(min(.x$Month)), month(min(.x$Month)))))
  )

#Generate predictions on the test data period using the ARIMA and Historical Mean models
predictions_test <- df_monthly_ts_train %>%
  mutate(
    arima_model = map(ts_data, ~auto.arima(.x, seasonal = TRUE)),
    
    #Historical Mean Model as ARIMA(0,0,0)
    mean_model = map(ts_data, ~Arima(.x, order = c(0, 0, 0))),
    
    #Generate forecasts specifically for the test data period
    arima_forecast_test = map2(arima_model, Hospital, ~{
      if (.y %in% df_monthly_test$Hospital) {
        forecast(.x, h = nrow(df_monthly_test[df_monthly_test$Hospital == .y, ]))
      } else {
        NULL
      }
    }),
    mean_forecast_test = map2(mean_model, Hospital, ~{
      if (.y %in% df_monthly_test$Hospital) {
        forecast(.x, h = nrow(df_monthly_test[df_monthly_test$Hospital == .y, ]))
      } else {
        NULL
      }
    })
  ) %>%
  select(Hospital, arima_forecast_test, mean_forecast_test) %>%
  filter(!is.null(arima_forecast_test) & !is.null(mean_forecast_test))

#Extract forecasted values for comparison with actual test data
predictions_plot_data <- predictions_test %>%
  mutate(
    arima_forecast_data = map2(Hospital, arima_forecast_test, ~{
      forecast_dates <- df_monthly_test %>%
        filter(Hospital == .x) %>%
        pull(Month)
      data.frame(
        Month = forecast_dates,
        Forecast = .y$mean,
        Model = "ARIMA",
        Hospital = .x
      )
    }),
    mean_forecast_data = map2(Hospital, mean_forecast_test, ~{
      forecast_dates <- df_monthly_test %>%
        filter(Hospital == .x) %>%
        pull(Month)
      data.frame(
        Month = forecast_dates,
        Forecast = .y$mean,
        Model = "Historical Mean",
        Hospital = .x
      )
    })
  ) %>%
  select(Hospital, arima_forecast_data, mean_forecast_data) %>%
  unnest(c(arima_forecast_data, mean_forecast_data), names_sep = "_")


test_plot_data <- df_monthly_test %>%
  rename(Actual = Sum_Total) %>%
  mutate(Model = "Actual")

plot_data_test <- bind_rows(
  predictions_plot_data %>%
    rename(Month = arima_forecast_data_Month) %>%
    select(Hospital = arima_forecast_data_Hospital, Month, Forecast = arima_forecast_data_Forecast, Model = arima_forecast_data_Model),
  
  predictions_plot_data %>%
    rename(Month = mean_forecast_data_Month) %>%
    select(Hospital = mean_forecast_data_Hospital, Month, Forecast = mean_forecast_data_Forecast, Model = mean_forecast_data_Model),
  
  test_plot_data %>%
    rename(Forecast = Actual)
)

#Plot the results comparing ARIMA, Historical Mean, and Actual test values
ggplot(plot_data_test, aes(x = Month, y = Forecast, color = Model)) +
  geom_line() +
  facet_wrap(~ Hospital, nrow = 5, ncol = 2, scales = "free_y") +
  labs(title = "ARIMA vs Historical Mean Predictions vs Actual on Test Data by Hospital",
       x = "Date",
       y = "Sum Total",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("ARIMA" = "blue", "Historical Mean" = "green", "Actual" = "black")) +
  theme(
    strip.text = element_text(size = 5),
    axis.text.x = element_text(size = 5),
    axis.text.y = element_text(size = 5),
    axis.title.x = element_text(size = 5),
    axis.title.y = element_text(size = 5)
  )


#MAPE with Naive MAPE

library(dplyr)
library(purrr)

#Calculate MAPE between the forecast and the actual test data
calculate_mape <- function(actual, forecast) {
  mean(abs((actual - forecast) / actual), na.rm = TRUE) * 100
}


actual_values <- df_monthly_test %>%
  rename(Actual = Sum_Total) %>%
  select(Hospital, Month, Actual)

#Create a naive model
create_naive_forecast <- function(train_data, test_data_length) {
  last_observed_value <- tail(train_data, 1)
  rep(last_observed_value, test_data_length)
}

#Generate naive forecasts for the test data period
predictions_test <- df_monthly_ts_train %>%
  mutate(
    arima_model = map(ts_data, ~auto.arima(.x, seasonal = TRUE)),
    
    #Generate Naive forecasts for each hospital
    naive_forecast_test = map2(ts_data, Hospital, ~{
      test_length <- nrow(df_monthly_test %>% filter(Hospital == .y))
      create_naive_forecast(.x, test_length)
    })
  ) %>%
  select(Hospital, arima_model, naive_forecast_test)

#Calculate MAPE for ARIMA and Naive models
mape_results <- predictions_test %>%
  mutate(
    arima_mape = map2_dbl(arima_model, Hospital, ~{
      actual_data <- actual_values %>%
        filter(Hospital == .y) %>%
        pull(Actual)
      forecasted_values <- forecast(.x, h = length(actual_data))$mean
      calculate_mape(actual_data, forecasted_values)
    }),
    
    naive_mape = map2_dbl(naive_forecast_test, Hospital, ~{
      actual_data <- actual_values %>%
        filter(Hospital == .y) %>%
        pull(Actual)
      forecasted_values <- .x
      calculate_mape(actual_data, forecasted_values)
    })
  ) %>%
  select(Hospital, arima_mape, naive_mape)

print(mape_results)

average_mape <- mape_results %>%
  summarise(
    avg_arima_mape = mean(arima_mape, na.rm = TRUE),
    avg_naive_mape = mean(naive_mape, na.rm = TRUE)
  )

print(average_mape)



#RANDOM FOREST FIT MODEL 
library(randomForest)
library(tidyr)
library(lubridate)

#Prepare the data with lag features
df_monthly_train <- train_data %>%
  group_by(Hospital, Month = floor_date(ArchiveDate, "month")) %>%
  summarise(Sum_Total = sum(Sum_Total, na.rm = TRUE), .groups = 'drop') %>%
  arrange(Hospital, Month) %>%
  group_by(Hospital) %>%
  mutate(
    Lag1 = lag(Sum_Total, 1),
    Lag2 = lag(Sum_Total, 2),
    Lag3 = lag(Sum_Total, 3)
  ) %>%
  na.omit()

df_monthly_test <- test_data %>%
  group_by(Hospital, Month = floor_date(ArchiveDate, "month")) %>%
  summarise(Sum_Total = sum(Sum_Total, na.rm = TRUE), .groups = 'drop') %>%
  arrange(Hospital, Month) %>%
  group_by(Hospital) %>%
  mutate(
    Lag1 = lag(Sum_Total, 1),
    Lag2 = lag(Sum_Total, 2),
    Lag3 = lag(Sum_Total, 3)
  ) %>%
  na.omit()

#Train Random Forest models for each hospital
fit_rf <- df_monthly_train %>%
  group_by(Hospital) %>%
  do(model = randomForest(Sum_Total ~ Lag1 + Lag2 + Lag3, data = ., ntree = 1000))

#Make predictions on test data
df_predictions <- df_monthly_test %>%
  group_by(Hospital) %>%
  do({
    hospital <- unique(.$Hospital)
    model <- fit_rf$model[[which(unique(df_monthly_train$Hospital) == hospital)]]
    pred <- predict(model, newdata = .)
    data.frame(Month = .$Month, Actual = .$Sum_Total, Predicted = pred)
  }) %>%
  ungroup()

#Calculate MAPE
df_predictions <- df_predictions %>%
  mutate(
    MAPE = abs((Actual - Predicted) / Actual) * 100
  )

#MAPE for each hospital
mape_summary <- df_predictions %>%
  group_by(Hospital) %>%
  summarise(
    Mean_MAPE = mean(MAPE, na.rm = TRUE),
    .groups = 'drop'
  )

print(mape_summary)

#Plot predictions against actual values
ggplot(df_predictions, aes(x = Month)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  facet_wrap(~ Hospital, nrow = 5, ncol = 2, scales = "free_y") +
  labs(title = "Actual vs Predicted Values",
       x = "Date",
       y = "Sum Total",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "black", "Predicted" = "blue"))


#VS NAIVE PREDICTION and MAPE comparison 

#Prepare the data with lag features
df_monthly_train <- train_data %>%
  group_by(Hospital, Month = floor_date(ArchiveDate, "month")) %>%
  summarise(Sum_Total = sum(Sum_Total, na.rm = TRUE), .groups = 'drop') %>%
  arrange(Hospital, Month) %>%
  group_by(Hospital) %>%
  mutate(
    Lag1 = lag(Sum_Total, 1),
    Lag2 = lag(Sum_Total, 2),
    Lag3 = lag(Sum_Total, 3)
  ) %>%
  na.omit()

df_monthly_test <- test_data %>%
  group_by(Hospital, Month = floor_date(ArchiveDate, "month")) %>%
  summarise(Sum_Total = sum(Sum_Total, na.rm = TRUE), .groups = 'drop') %>%
  arrange(Hospital, Month) %>%
  group_by(Hospital) %>%
  mutate(
    Lag1 = lag(Sum_Total, 1),
    Lag2 = lag(Sum_Total, 2),
    Lag3 = lag(Sum_Total, 3)
  ) %>%
  na.omit()

#Train Random Forest models for each hospital
fit_rf <- df_monthly_train %>%
  group_by(Hospital) %>%
  do(model = randomForest(Sum_Total ~ Lag1 + Lag2 + Lag3, data = ., ntree = 1000))

#Make predictions on test data
df_predictions <- df_monthly_test %>%
  group_by(Hospital) %>%
  do({
    hospital <- unique(.$Hospital)
    model <- fit_rf$model[[which(unique(df_monthly_train$Hospital) == hospital)]]
    pred_rf <- predict(model, newdata = .)
    last_observed <- .$Lag1  # Naive prediction using the last observed value
    data.frame(Month = .$Month, Actual = .$Sum_Total, Predicted_RF = pred_rf, Predicted_Naive = last_observed)
  }) %>%
  ungroup()

#Calculate MAPE for both Random Forest and Naive predictions
df_predictions <- df_predictions %>%
  mutate(
    MAPE_RF = abs((Actual - Predicted_RF) / Actual) * 100,
    MAPE_Naive = abs((Actual - Predicted_Naive) / Actual) * 100
  )

#MAPE for each hospital
mape_summary <- df_predictions %>%
  group_by(Hospital) %>%
  summarise(
    Mean_MAPE_RF = mean(MAPE_RF, na.rm = TRUE),
    Mean_MAPE_Naive = mean(MAPE_Naive, na.rm = TRUE),
    .groups = 'drop'
  )

print(mape_summary)

#Plot predictions against actual values for both models
ggplot(df_predictions, aes(x = Month)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Predicted_RF, color = "Predicted_RF")) +
  geom_line(aes(y = Predicted_Naive, color = "Predicted_Naive")) +
  facet_wrap(~ Hospital, nrow = 5, ncol = 2, scales = "free_y") +
  labs(title = "Actual vs Predicted Values (RF vs Naive)",
       x = "Date",
       y = "Sum Total",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "black", "Predicted_RF" = "blue", "Predicted_Naive" = "red"))


#RF FORECAST
library(zoo) 

df_monthly_train <- train_data %>%
  group_by(Hospital, Month = floor_date(ArchiveDate, "month")) %>%
  summarise(Sum_Total = sum(Sum_Total, na.rm = TRUE), .groups = 'drop')

df_monthly_test <- test_data %>%
  group_by(Hospital, Month = floor_date(ArchiveDate, "month")) %>%
  summarise(Sum_Total = sum(Sum_Total, na.rm = TRUE), .groups = 'drop')

df_monthly_train <- df_monthly_train %>%
  arrange(Hospital, Month) %>%
  group_by(Hospital) %>%
  mutate(
    Lag1 = lag(Sum_Total, 1),
    Lag2 = lag(Sum_Total, 2),
    Lag3 = lag(Sum_Total, 3)
  ) %>%
  na.omit()

df_monthly_test <- df_monthly_test %>%
  arrange(Hospital, Month) %>%
  group_by(Hospital) %>%
  mutate(
    Lag1 = lag(Sum_Total, 1),
    Lag2 = lag(Sum_Total, 2),
    Lag3 = lag(Sum_Total, 3)
  ) %>%
  na.omit()

#Fit the Random Forest model for each hospital
fit_rf <- df_monthly_train %>%
  group_by(Hospital) %>%
  do(model = randomForest(Sum_Total ~ Lag1 + Lag2 + Lag3, data = ., ntree = 100))

#Prepare future dates for forecasting
future_start <- ymd("2024-07-25")
future_end <- ymd("2026-07-25")
future_dates <- seq.Date(from = future_start, to = future_end, by = "month")
df_future <- expand.grid(Hospital = unique(df_monthly_train$Hospital), Month = future_dates)

#Combine historical and future data
df_combined <- bind_rows(
  df_monthly_train %>% select(Hospital, Month, Sum_Total, Lag1, Lag2, Lag3) %>% mutate(Source = "Historical"),
  df_monthly_test %>% select(Hospital, Month, Sum_Total) %>% mutate(Source = "Test"),
  df_future %>% mutate(Lag1 = NA, Lag2 = NA, Lag3 = NA, Sum_Total = NA, Forecast = NA, Source = "Forecast")
)

#Handle missing Lag values by forward-filling
df_combined <- df_combined %>%
  group_by(Hospital) %>%
  arrange(Month) %>%
  mutate(
    Lag1 = zoo::na.locf(Lag1, na.rm = FALSE),
    Lag2 = zoo::na.locf(Lag2, na.rm = FALSE),
    Lag3 = zoo::na.locf(Lag3, na.rm = FALSE)
  ) %>%
  ungroup()

df_future_predictions <- df_combined %>%
  filter(Source == "Forecast") %>%
  mutate(Forecast = NA)  # Initialize Forecast column

#Perform iterative forecasting
for (hospital in unique(df_combined$Hospital)) {
  # Extract the relevant data
  df_hospital <- df_combined %>%
    filter(Hospital == hospital) %>%
    arrange(Month)
  
  #Get the initial lags from the last available month in historical data
  initial_lags <- df_hospital %>%
    filter(Month == max(df_monthly_train$Month)) %>%
    select(Lag1, Lag2, Lag3) %>%
    unlist() %>%
    as.numeric()
  
  #Fit the model
  model <- fit_rf$model[[which(unique(df_monthly_train$Hospital) == hospital)]]
  
  #Iterate over the future periods
  for (i in 1:nrow(df_future %>% filter(Hospital == hospital))) {
    
    new_data <- data.frame(Lag1 = initial_lags[1], Lag2 = initial_lags[2], Lag3 = initial_lags[3])
    
    #Predict the next value
    next_pred <- predict(model, newdata = new_data)
    
  
    current_date <- future_dates[i]
    df_future_predictions_row <- df_future_predictions %>%
      filter(Hospital == hospital, Month == current_date)
    
    if (nrow(df_future_predictions_row) > 0) {
      df_future_predictions$Forecast[df_future_predictions$Hospital == hospital & df_future_predictions$Month == current_date] <- next_pred
    }
    
    #Update lag values
    initial_lags <- c(next_pred, initial_lags[1:2])
  }
}


df_combined <- df_combined %>%
  left_join(df_future_predictions %>% select(Hospital, Month, Forecast), by = c("Hospital", "Month")) %>%
  mutate(Forecast = coalesce(Forecast.x, Forecast.y)) %>%
  select(-Forecast.x, -Forecast.y)

#Plot historical data and forecasted values
ggplot(df_combined, aes(x = Month)) +
  geom_line(data = filter(df_combined, Source == "Historical"), aes(y = Sum_Total, color = "Historical"), size = 1) +
  geom_line(data = filter(df_combined, Source == "Test"), aes(y = Sum_Total, color = "Test"), size = 1) +
  geom_line(data = filter(df_combined, Source == "Forecast"), aes(y = Forecast, color = "Forecast"), size = 1) +
  facet_wrap(~ Hospital, nrow = 5, ncol = 2, scales = "free_y") +
  labs(title = "Historical Data, Test Data, and Forecasted Values",
       x = "Date",
       y = "Sum Total",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Historical" = "black", "Test" = "red", "Forecast" = "blue")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) 


#NAIVE FORECAST

df_monthly_train <- train_data %>%
  group_by(Hospital, Month = floor_date(ArchiveDate, "month")) %>%
  summarise(Sum_Total = sum(Sum_Total, na.rm = TRUE), .groups = 'drop')

df_monthly_test <- test_data %>%
  group_by(Hospital, Month = floor_date(ArchiveDate, "month")) %>%
  summarise(Sum_Total = sum(Sum_Total, na.rm = TRUE), .groups = 'drop')

#Prepare future dates for forecasting
future_start <- ymd("2024-07-25")
future_end <- ymd("2026-07-25")
future_dates <- seq.Date(from = future_start, to = future_end, by = "month")
df_future <- expand.grid(Hospital = unique(df_monthly_train$Hospital), Month = future_dates)

#Combine historical and future data
df_combined <- bind_rows(
  df_monthly_train %>% select(Hospital, Month, Sum_Total) %>% mutate(Forecast = NA, Source = "Historical"),
  df_monthly_test %>% select(Hospital, Month, Sum_Total) %>% mutate(Forecast = NA, Source = "Test Data"),
  df_future %>% mutate(Sum_Total = NA, Forecast = NA, Source = "Forecast")
)

df_future_predictions <- df_combined %>%
  filter(Month >= future_start) %>%
  mutate(Forecast = NA)  # Initialize Forecast column

#Perform naive forecasting
for (hospital in unique(df_combined$Hospital)) {
  # Extract the relevant historical data
  df_hospital <- df_combined %>%
    filter(Hospital == hospital) %>%
    arrange(Month)
  

  last_value <- df_hospital %>%
    filter(!is.na(Sum_Total)) %>%
    slice_tail(n = 1) %>%
    pull(Sum_Total)
  
  #Iterate over the future periods
  for (i in 1:nrow(df_future %>% filter(Hospital == hospital))) {
    current_date <- future_dates[i]
    df_future_predictions_row <- df_future_predictions %>%
      filter(Hospital == hospital, Month == current_date)
    
    if (nrow(df_future_predictions_row) > 0) {
      df_future_predictions$Forecast[df_future_predictions$Hospital == hospital & df_future_predictions$Month == current_date] <- last_value
    }
  }
}

#Combine historical, test data, and forecasted data
df_all <- bind_rows(
  df_monthly_train %>%
    mutate(Source = "Historical"),
  df_monthly_test %>%
    mutate(Source = "Test Data"),
  df_future_predictions %>%
    mutate(Source = "Naive Forecast")
)

#Plot historical data, test data, and forecasted values
ggplot(df_all, aes(x = Month)) +
  geom_line(data = filter(df_all, Source == "Historical"), aes(y = Sum_Total, color = "Historical"), size = 1) +
  geom_line(data = filter(df_all, Source == "Test Data"), aes(y = Sum_Total, color = "Test Data"), size = 1, linetype = "dashed") +
  geom_line(data = filter(df_all, Source == "Naive Forecast"), aes(y = Forecast, color = "Naive Forecast"), size = 1) +
  facet_wrap(~ Hospital, nrow = 5, ncol = 2, scales = "free_y") +
  labs(title = "Historical Data, Test Data, and Naive Forecasted Values",
       x = "Date",
       y = "Sum Total",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Historical" = "black", "Test Data" = "red", "Naive Forecast" = "blue")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) 







