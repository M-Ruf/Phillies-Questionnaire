# Title:    Phillies Performance Analyst Questionnaire
# File:     OBP data.xlsx
# Author:   Melissa Ruf

# INSTALL AND LOAD PACKAGES ################################

# Install tidyverse and caret
if(!require("pacman")) install.packages(c("pacman"))
pacman::p_load(
     GGally, 
     magrittr, 
     pacman, 
     rio, 
     tidyverse, 
     caret,
     dplyr)


# Question 7: Lottery ticket

# Part A
ticket_value <- rnorm(1, mean = 50, sd = 50)
ticket_mean <- 50
ticket_sd <- 50

p_less_than_one <- (1-ticket_mean) / ticket_sd
p_greater_than_hundred <- 1 - ((100 - ticket_mean) / ticket_sd)
p_inbtween <- p_greater_than_hundred - p_less_than_one

expected_value <- (p_less_than_one*1) + (p_inbtween*ticket_value) + (p_greater_than_hundred * (ticket_value+20))
expected_value
     # [1] 9.204843

# Part B
ticket_draw <- function() {
     ticket_value <- rnorm(1, mean = 50, sd = 50)
     
     if (ticket_value < 1) {
          return(1)
     } else if (ticket_value > 100) {
          return(ticket_value + 20)
     } else {
          return(ticket_value)
     }
}

entry_sim <- function() {
     ticket1 <- ticket_draw()
     ticket2 <- ticket_draw()
     
     max_value <- max(ticket1,ticket2)
     
     return(max_value)
}

# Simulation for 1 ticket

simulations_one_ticket <- 1000000
one_ticket_values <- replicate(simulations_one_ticket, ticket_draw())

# Simulation for entry
simulations_one_entry <- 1000000
one_entry_values <- replicate(simulations_one_entry, entry_sim())

# Expected values
expected_value_one_ticket <- mean(one_ticket_values)
expected_value_one_entry <- mean(one_entry_values)

# Results
cat("Expected Value of One Ticket:", expected_value_one_ticket, "\n")  
     #Expected Value of One Ticket: 57.41327 

cat("Expected Value of an Entry:", expected_value_one_entry, "\n")
     #Expected Value of an Entry: 84.48581 



# Question 11: Predicted OBP
# FILE UPLOAD ##############################################

df_obp <- import("C:/Users/melis/OneDrive/Desktop/obp data.xlsx") %>% 
    
     print()


# DATA EXPLORATION #########################################

df_obp %>% summary()
df_obp %>% str()
df_obp %>% head()

# DATA WRANGLING  ##########################################

#Average OBP of each player , omit missing values
# Add average obp per player to df

df_obp <- df_obp %>%
     group_by(playerid) %>% 
     mutate(avg_obp = mean(c(OBP_16, OBP_17, OBP_18, OBP_19, OBP_20), na.rm = TRUE)) %>% 
     print()
     
# Replace all OBP missing values with the player's average OBP for the column 'OBP_16'

df_obp %<>% 
     group_by(playerid) %>% 
     mutate(
          OBP_16 = ifelse(
               is.na(OBP_16), 
               avg_obp, 
               OBP_16
               )
          )

head(df_obp)

# Repeat for remaining columns OBP_17 to OBP_20
df_obp %<>% 
     group_by(playerid) %>% 
     mutate(
          OBP_17 = ifelse(
               is.na(OBP_17), 
               avg_obp, 
               OBP_17
          )
     )


     
df_obp %<>% 
     group_by(playerid) %>% 
     mutate(
          OBP_18 = ifelse(
               is.na(OBP_18), 
               avg_obp, 
               OBP_18
          )
     )


df_obp %<>% 
     group_by(playerid) %>% 
     mutate(
          OBP_19 = ifelse(
               is.na(OBP_19), 
               avg_obp, 
               OBP_19
          )
     )

df_obp %<>% 
     group_by(playerid) %>% 
     mutate(
          OBP_20 = ifelse(
               is.na(OBP_20), 
               avg_obp, 
               OBP_20
          )
     )

head(df_obp)


#Average PA of each player , omit missing values
# Add average pa per player to df

df_obp <- df_obp %>%
     group_by(playerid) %>% 
     mutate(avg_pa = mean(c(PA_16, PA_17, PA_18, PA_19, PA_20), na.rm = TRUE, digits=0)) %>% 
     print()

# Replace all PA missing values with the player's average PA

df_obp %<>% 
     group_by(playerid) %>% 
     mutate(
          PA_16 = ifelse(
               is.na(PA_16), 
               avg_pa, 
               PA_16
          )
     )

head(df_obp)

df_obp %<>% 
     group_by(playerid) %>% 
     mutate(
          PA_17 = ifelse(
               is.na(PA_17), 
               avg_pa, 
               PA_17
          )
     )

df_obp %<>% 
     group_by(playerid) %>% 
     mutate(
          PA_18 = ifelse(
               is.na(PA_18), 
               avg_pa, 
               PA_18
          )
     )

df_obp %<>% 
     group_by(playerid) %>% 
     mutate(
          PA_19 = ifelse(
               is.na(PA_19), 
               avg_pa, 
               PA_19
          )
     )

df_obp %<>% 
     group_by(playerid) %>% 
     mutate(
          PA_20 = ifelse(
               is.na(PA_20), 
               avg_pa, 
               PA_20
          )
     )

head(df_obp)

view(df_obp)

# Remove any players with no previous obp or pa data

df_obp_clean <- df_obp[complete.cases(df_obp),]
view(df_obp_clean) 
head(df_obp_clean)


# Round OBP to thousands
df_obp_clean <- df_obp_clean %>% 
     mutate(across(starts_with("OBP"), ~round(., 3)))

# Round PA to zero
df_obp_clean <- df_obp_clean %>% 
     mutate(across(starts_with("PA"), ~round(., 0)))

df_obp_clean <- df_obp_clean %>% 
     mutate(avg_pa = round(avg_pa, 0))

# Select final desired columns
head(df_obp_clean)
df_obp_clean <- df_obp_clean %>% 
     select(Name, playerid, PA_16, OBP_16, PA_17, OBP_17, PA_18, OBP_18,
            PA_19, OBP_19, PA_20, OBP_20, PA_21, OBP_21, avg_obp)

view(df_obp_clean)

# Linear Regression
lm1 <- lm(OBP_21 ~ PA_16 + PA_17 + PA_18 + PA_19 + PA_20 + 
          OBP_16 + OBP_17 + OBP_18 + OBP_19 + OBP_20, 
          data = df_obp_clean)
summary(lm1)

# Residual standard error: 0.04551 on 495 degrees of freedom
# Multiple R-squared:  0.1605,	Adjusted R-squared:  0.1436 
# F-statistic: 9.465 on 10 and 495 DF,  p-value: 1.833e-14

# Remodel using significant variables: PA_17, PA_19, PA_20, OBP_20

lm2 <- lm(OBP_21 ~ PA_17 + PA_19 + PA_20 + OBP_20, 
          data = df_obp_clean)
summary(lm2)

# Residual standard error: 0.04544 on 501 degrees of freedom
# Multiple R-squared:  0.153,	Adjusted R-squared:  0.1462 
# F-statistic: 22.62 on 4 and 501 DF,  p-value: < 2.2e-16

# Remodel using significant variables: PA_19, PA_20, OBP_20
lm3 <- lm(OBP_21 ~ PA_19 + PA_20 + OBP_20, 
          data = df_obp_clean)

#Summarize regression model
summary(lm3)

# Residual standard error: 0.04545 on 502 degrees of freedom
# Multiple R-squared:  0.1508,	Adjusted R-squared:  0.1458 
# F-statistic: 29.72 on 3 and 502 DF,  p-value: < 2.2e-16

# Confidence intervals for coefficients
lm3 %>% confint()

# Regression diagnostics
lm3 %>% lm.influence()
lm3 %>% influence.measures()

# Plots
plot(lm3)

# Set randon seed for reproducibility
set.seed(333)

# SPLIT THE DATA #############
index <- createDataPartition(df_obp_clean$OBP_21, p=0.7, list = FALSE)
train_data <- df_obp_clean[index, ]
test_data <- df_obp_clean[-index, ]

# Use lm3 to predict training data
preds_train <- lm3 %>% 
     predict(newdata = train_data)
preds_train <- round(preds_train, 3)

# Actual values within training data
actual_train <- train_data$OBP_21

# Calculate Mean Absolute Error
mae <- mean(abs(preds_train - actual_train))
mae  # 0.035

# Calculate Mean Squared Error
mse <- mean((preds_train - actual_train)^2)
mse  # 0.002

# Calculate Root Mean Squared Error
rmse <- sqrt(mse)
rmse  # 0.045



# Use lm3 to predict test data
preds_test <- lm3 %>% 
     predict(newdata = test_data)
preds_test

# Actual values within test data
actual_test <- test_data$OBP_21

# Calculate Mean Absolute Error
mae <- mean(abs(preds_test - actual_test))
mae  # 0.036

# Calculate Mean Squared Error
mse <- mean((preds_test- actual_test)^2)
mse  # 0.002

# Calculate Root Mean Squared Error
rmse <- sqrt(mse)
rmse  # 0.046


# Use lm3 on clean obp data
preds <- lm3 %>% 
     predict(newdata = df_obp_clean)
preds 

length(preds)

# Add predicted values to df_obp_clean

df_obp_clean %>% 
     ungroup() %>% 
     mutate(predicted_values = fitted(lm3)) %>% 
     print() %>% 
     view()

# Interpretation

# The low MAE, MSE, and RMSE values indicate good predictive performance
# The R-squared values for lm3 indicates that the model explains a moderate 
#    percentage of the variability in the dependent variable
# The significant F-statistic and very low p-value suggest the model is
#    statistically significant overall





























