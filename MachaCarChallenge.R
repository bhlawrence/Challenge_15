### DELIVERABLE 1

# Import library dplyr
library(dplyr)

# Import and read the MechaCar_mpg.csv
MechaCar_data <- read.csv("Resources/MechaCar_mpg.csv")

# Perform linear regression on MechaCar and pass all 6 columns through
regression <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechaCar_data)

#find p-value and r-squared for each value in linear regression model.
summary(regression)

### DELIVERABLE 2

#import and read the suspension_coil.csv
suspension_coil_data <- read.csv("Resources/Suspension_Coil.csv")

# create a total_summary dataframe using the summarize() function to get the mean, median, variance and standard deviation of the spension coils psi system
total_summary <- suspension_coil_data %>% summarize(Mean=mean(PSI), , Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = 'keep')

# create a group by lot summary of the same data.
lot_summary <- suspension_coil_data %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), , Median=median(PSI), Variance=var(PSI), SD=sd(PSI), .groups = 'keep')

## Deliverable 3

# t-test to determine if the PSI across all manufacturing lots is statistically different from the population mean of 1500
t.test(suspension_coil_data$PSI, mu=1500)

# t-test each lot to determine the same as above

#lot 1
t.test(subset(suspension_coil_data, Manufacturing_Lot == "Lot1", select = PSI), mu=1500)

#lot 2
t.test(subset(suspension_coil_data, Manufacturing_Lot == "Lot2", select = PSI), mu=1500)

#lot 3
t.test(subset(suspension_coil_data, Manufacturing_Lot == "Lot3", select = PSI), mu=1500)

