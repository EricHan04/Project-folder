# #load to shinyapps.io
# install.packages('rsconnect')
# 
# rsconnect::setAccountInfo(name='erichan',
#                           token='BC36317C639CF38DD78DE0E9BCBF00AB',
#                           secret='<SECRET>')
# library(rsconnect)
# rsconnect::deployApp('C:\Users\EricHan\Project-folder-1\Rshiny Beach tool valuation\')

# Load packages ----------------------------------------------------------------

library(shiny)
library(htmltools)
library(dplyr)
library(DT)
library(shinyjs)
library(ggplot2)
library(waterfalls)
library(plotly)
library(readr)
library(stringr)
library(scales)

#Create the table headers used by output table

table_frame1 <-
  function() {
    htmltools::withTags(table(class = 'display',
                              thead(
                                tr(
                                  # th(rowspan = 3, 'LGA'),
                                  th(rowspan = 3, 'Beach', icon("circle-info", lib = "font-awesome", class="temp", title="Beaches are listed in geographical order from North to South.")),
                                  th(class = 'dt-center',colspan=7, "Beach attributes" , icon("circle-info", lib = "font-awesome", class="temp", title="Beach width, length and the presence of a lifeguard service attributes have the most significant impacts on beach values.")),
                                  th(class = 'dt-center',colspan=6, "LGA boundary of analysis" , icon("circle-info", lib = "font-awesome", class="temp", title="The local government area (LGA) is the boundary of analysis for coast management option cost-benefit analysis."), tags$button(id="lgachart", "Net LGA Impact Chart",class="action-button", style="float:right; margin-left:10px", icon("circle-info", lib = "font-awesome", class="temp", title="View NET LGA boundary of analysis impacts chart")),tags$button(id="lga_visitation", "Visitation Output",class="action-button", style="float:right", icon("circle-info", lib = "font-awesome", class="temp", title="Visitation data for underlying beach consumer surplus and producer surplus."))),

                                ),
                                tr(
                                  th(class = 'dt-center', colspan = 3, 'Current state', icon("circle-info", lib = "font-awesome", class="temp", title="Values of beaches in their current state. The base case values may differ from the current state values if the beach width, length and/or the presence of a patrol service is expected to change under base case management activities")),
                                  th(class = 'dt-center', colspan = 4, 'Scenario', icon("circle-info", lib = "font-awesome", class="temp", title="Enter alternative figures here to 1) estimate base case values if beach attributes are expected to differ under current management activities compared with the current state of the beach; and 2) provide first pass estimate for use in rapid CBA tool calculations or prelimnary cost-benefit analysis. Monte-Carlo geomorphology probability of impact data is required to estimate coastal management option's impact (see section X in the user guide)."),tags$button(id="calculate",class="action-button", style="float:right; margin-left:10px", "Calculate", icon("circle-info", lib = "font-awesome", class="temp", title="Generate value estimates if you have entered alternative values for beach width, length and/or presence of a lifeguard service."), tags$button(id="reset", "Reset",class="action-button", style="float:right", icon("circle-info", lib = "font-awesome", class="temp", title="Remove any alternative values entered in the scenario section to estimate values impacts of another scenario.")),
                                  ),
                                  ),
                                  th(class = 'dt-center', colspan = 3, 'Consumer surplus', icon("circle-info", lib = "font-awesome", class="temp", title="Estimates beach visits value.")),
                                  th(class = 'dt-center', colspan = 3, 'Producer surplus' , icon("circle-info", lib = "font-awesome", class="temp", title="Estimates tourist related business beach visits value.")),
                                  tr(lapply(rep(c('Width', 'Length', 'Patrolled'), 1), th),
                                     lapply(rep(c('Width', 'Length', 'Patrolled','Closed'), 1), th),
                                     (lapply(rep(
                                       c('Current state', 'Scenario', 'Change'), 2
                                     ), th))),
                                )
                              )))
  }

table_visitation_lga<-
  function() {
    htmltools::withTags(table(class = 'display',
                              thead(
                                tr(
                                  th(rowspan = 3, 'Beach', icon("circle-info", lib = "font-awesome", class="temp", title="Beaches are listed in geographical order from North to South.")),
                                  th(class = 'dt-center',colspan=16, "Visitors by origin"),
                                ),
                                tr(
                                  th(class = 'dt-center',colspan=7, "Current State"),
                                  th(class = 'dt-center',colspan=7, "Scenario"),
                                  th(class = 'dt-center',colspan=2, "Change"),
                                ),
                                tr(
                                  lapply(class='dt-center', rep(
                                    c('LGA','Intra-state','Interstate','International','Total', '$CS/person', '$PS/person'), 2
                                  ), th),
                                  th(class = 'dt-center', '#'),
                                  th(class = 'dt-center', '%'),
                                ),
                              )
    ))
  }

table_visitation_state<-
  function() {
    htmltools::withTags(table(class = 'display',
                              thead(
                                tr(
                                  th(rowspan = 3, 'Beach', icon("circle-info", lib = "font-awesome", class="temp", title="Beaches are listed in geographical order from North to South.")),
                                  th(class = 'dt-center',colspan=16, "Visitors by origin"),
                                ),
                                tr(
                                  th(class = 'dt-center',colspan=7, "Current State"),
                                  th(class = 'dt-center',colspan=7, "Scenario"),
                                  th(class = 'dt-center',colspan=2, "Change"),
                                ),
                                tr(
                                  lapply(class='dt-center', rep(
                                    c('LGA','Intra-state','Interstate','International','Total', '$CS/person', '$PS/person'), 2
                                  ), th),
                                  th(class = 'dt-center', '#'),
                                  th(class = 'dt-center', '%'),
                                ),
                              )
    ))
  }


table_frame2 <-
  function() {
    htmltools::withTags(table(class = 'display',
                              thead(
                                tr(
                                  th(rowspan = 3, 'Beach', icon("circle-info", lib = "font-awesome", class="temp", title="Beaches are listed in geographical order from North to South.")),
                                  th(class = 'dt-center',colspan=7, "Beach attributes" , icon("circle-info", lib = "font-awesome", class="temp", title="Beach width, length and the presence of a lifeguard service attributes have the most significant impacts on beach values.")),
                                  th(class = 'dt-center',colspan=6, "State boundary of analysis" , icon("circle-info", lib = "font-awesome", class="temp", title="The NSW State border is the boundary of analysis for coast management option cost-benefit analysis."),tags$button(id="statechart", "Net State Impact Chart",class="action-button", style="float:right; margin-left:10px", icon("circle-info", lib = "font-awesome", class="temp", title="View NET State boundary of analysis impacts chart")),tags$button(id="state_visitation", "Visitation Output",class="action-button", style="float:right", icon("circle-info", lib = "font-awesome", class="temp", title="Visitation data for underlying beach consumer surplus and producer surplus."))),
                                  
                                ),
                                tr(
                                  th(class = 'dt-center', colspan = 3, 'Current state', icon("circle-info", lib = "font-awesome", class="temp", title="Values of beaches in their current state. The base case values may differ from the current state values if the beach width, length and/or the presence of a patrol service is expected to change under base case management activities")),
                                  th(class = 'dt-center', colspan = 4, 'Scenario', icon("circle-info", lib = "font-awesome", class="temp", title="Enter alternative figures here to 1) estimate base case values if beach attributes are expected to differ under current management activities compared with the current state of the beach; and 2) provide first pass estimate for use in rapid CBA tool calculations or prelimnary cost-benefit analysis. Monte-Carlo geomorphology probability of impact data is required to estimate coastal management option's impact (see section X in the user guide).")),
                                  th(class = 'dt-center', colspan = 3, 'Consumer surplus', icon("circle-info", lib = "font-awesome", class="temp", title="Estimates beach visits value.")),
                                  th(class = 'dt-center', colspan = 3, 'Producer surplus' , icon("circle-info", lib = "font-awesome", class="temp", title="Estimates tourist related business beach visits value.")),
                                  tr(lapply(rep(c('Width', 'Length', 'Patrolled'), 1), th),
                                     lapply(rep(c('Width', 'Length', 'Patrolled','Closed'), 1), th),
                                     (lapply(rep(
                                       c('Current state', 'Scenario', 'Change'), 2
                                     ), th))),
                                )
                              )))
  }


ebo <-
  function() {
    htmltools::withTags(table(class = 'display',
                              thead(
                                tr(
                                  th(rowspan = 3, 'Beach', icon("circle-info", lib = "font-awesome", class="temp", title="Beaches are listed in geographical order from North to South.")),
                                  th(class = 'dt-center',colspan=7, "Beach attributes" , icon("circle-info", lib = "font-awesome", class="temp", title="Beach width, length and the presence of a lifeguard service attributes have the most significant impacts on beach values.")),
                                  th(class = 'dt-center',colspan=6, "Existence, Bequest, Option to use Value (Annual)" , icon("circle-info", lib = "font-awesome", class="temp", title="Estimates existence, bequest/inheritence, option to user in the future value.")),
                                ),
                                tr(
                                  th(class = 'dt-center', colspan = 3, 'Current state', icon("circle-info", lib = "font-awesome", class="temp", title="Values of beaches in their current state. The base case values may differ from the current state values if the beach width, length and/or the presence of a patrol service is expected to change under base case management activities")),
                                  th(class = 'dt-center', colspan = 4, 'Scenario', icon("circle-info", lib = "font-awesome", class="temp", title="Enter alternative figures here to 1) estimate base case values if beach attributes are expected to differ under current management activities compared with the current state of the beach; and 2) provide first pass estimate for use in rapid CBA tool calculations or prelimnary cost-benefit analysis. Monte-Carlo geomorphology probability of impact data is required to estimate coastal management option's impact (see section X in the user guide).")),
                                  th(class = 'dt-center', colspan = 3, 'LGA Boundary', icon("circle-info", lib = "font-awesome", class="temp", title="The local government area (LGA) is the boundary of analysis.")),
                                  th(class = 'dt-center', colspan = 3, 'State Boundary', icon("circle-info", lib = "font-awesome", class="temp", title="The NSW State border is the boundary of analysis.")),
                                  tr(lapply(rep(c('Width', 'Length', 'Patrolled'), 1), th),
                                     lapply(rep(c('Width', 'Length', 'Patrolled','Closed'), 1), th),
                                     (lapply(rep(
                                       c('Current state', 'Scenario', 'Change'), 2
                                     ), th))),
                                )
                              )))
  }

data <- read.csv("1. interface_data.csv", stringsAsFactors = FALSE)

plotfont <- list(
  family = "'Public Sans', Arial, sans-serif")

library(dplyr)
library(readr) 

options(digits = 19)

# #load the interface data for the dashboard 
# data <- read.csv("1. interface_data.csv", stringsAsFactors = FALSE)

# Step 1: calculate the total visitation
# load LGA characteristics
LGA_data <- read.csv("A.1 LGA data.csv", encoding = "iso-8859-1")
colnames(LGA_data) <- c("LGA_Name", "2016_Population", "2021_Actual_Population",
                        "2023_Projected_Population", "2011_average_weekly_income",
                        "2021_average_weekly_income", "SEIFA")
LGA_data$Income_normalize <- LGA_data$'2021_average_weekly_income' / max(LGA_data$'2021_average_weekly_income')
LGA_data$SEIFA_normalize <- LGA_data$SEIFA / max(LGA_data$SEIFA)

BASE_data <- read.csv("A.3 BaseData.csv", encoding = "iso-8859-1")
colnames(BASE_data) <- c("Beach_FID", "Column1", "Beach_Name", "LGA_Name", "New_Count", "CONF", "Patrolled_B",
                         "Iconic", "Main", "National_Park", "Open_Coast", "Surf", "Bay",
                         "Length_B", "Width_B", "Latitude", "Longitude")

BASE_data <- merge(BASE_data,
                   LGA_data[c("LGA_Name", "2021_average_weekly_income",
                              "2023_Projected_Population", "SEIFA")],
                   by = "LGA_Name",
                   all.x = TRUE)

rm(LGA_data)
gc()

# load distance
cbd_dist <- read.csv("cbd_distance_corrected.csv", encoding = "iso-8859-1")
colnames(cbd_dist) <- c("LGA_FID", "LGA_Name", "Beach_FID", "Beach_Name", "Driving_d")

print(nrow(BASE_data))
BASE_data <- merge(BASE_data,
                   cbd_dist[c("Beach_FID", "Driving_d")],
                   by = "Beach_FID",
                   all.x = TRUE)
print(nrow(BASE_data))
rm(LGA_data)
gc()

#calculate independent variables:
BASE_data$N_LENGTH <- (BASE_data$`Length_B` - min(BASE_data$`Length_B`)) / 
  (max(BASE_data$`Length_B`)) - min(BASE_data$`Length_B`)

BASE_data$N_WIDTH <- (BASE_data$`Width_B` - min(BASE_data$`Width_B`)) / 
  (max(BASE_data$`Width_B`)) - min(BASE_data$`Width_B`)

BASE_data$N_INCOME <- (BASE_data$'2021_average_weekly_income' - min(BASE_data$'2021_average_weekly_income')) / 
  (max(BASE_data$'2021_average_weekly_income') - min(BASE_data$'2021_average_weekly_income'))

BASE_data$N_POP <- (BASE_data$'2023_Projected_Population' - min(BASE_data$'2023_Projected_Population')) / 
  (max(BASE_data$'2023_Projected_Population') - min(BASE_data$'2023_Projected_Population'))

BASE_data$ln_DIST <- log(BASE_data$Driving_d)

#create the fitted value using the params from the iterations
params_frame <- read.csv("benefit_transfer_params.csv", encoding = "iso-8859-1")

independent_vars <- c("Iconic", "National_Park", "Surf", "N_LENGTH", "Main", "Patrolled_B", "Bay",
                      "N_WIDTH", "N_POP", "N_INCOME", "ln_DIST")
BASE_data$const <- 1

independent_data <- as.matrix(BASE_data[, c("const", independent_vars)])
coeff <- as.numeric(params_frame$coeff)

predictions <- independent_data %*% coeff
BASE_data$predictions <- as.vector(predictions)

# revert the fitted value (LN of the raw number) to the total visitation number
BASE_data$total_visit_base <- exp(BASE_data$predictions)

# replace the total visitation calculated with the actual number if the actual number is larger than zero
# and has "A" or "B" confidence level
subset_condition <- !is.na(BASE_data$New_Count) & BASE_data$New_Count > 0 &
  (BASE_data$CONF == "A" | BASE_data$CONF == "B")
replacement_values <- BASE_data$New_Count[subset_condition]
BASE_data$total_visit_base[subset_condition] <- replacement_values

print(sum(BASE_data$total_visit_base))

# Step 2 - calculate international and interstate
## load data 

INT_visit_data <- read.csv("INT_visit_data.csv", encoding = "iso-8859-1")
INT_visit_data <- INT_visit_data[, !(colnames(INT_visit_data) %in% c("International.Visitors", "Interstate.Visitors"))]
INT_visit_data$Beach_FID <- INT_visit_data$Beach_FID + 1

#get the total visitation number from step 1 and merge it to the LGA/Beach frame
base1 <- BASE_data[c("Beach_FID", "total_visit_base")]
colnames(base1) <- c("Beach_FID", "total_visit_base1")
INT_visit_data <- merge(INT_visit_data, base1, by = "Beach_FID", all.x = TRUE)
INT_visit_data$Beach_availability <- 1

#get the actual number of international and interstate number
base2 <- read.csv("international_interstate_visitors_number.csv", encoding = "iso-8859-1")
base2_1 <- base2[c(1, 7)]
colnames(base2_1) <- c("LGA_FID", "international_visitors")
base2_1 <- base2_1[3:130, ]
base2_1$international_visitors <- as.numeric(gsub("NP", "0.001", base2_1$international_visitors)) * 1000
base2_1$LGA_FID <- as.integer(base2_1$LGA_FID)

base2_2 <- base2[c(1, 11)]
colnames(base2_2) <- c("LGA_FID", "interstate_visitors")
base2_2 <- base2_2[3:130, ]
base2_2$interstate_visitors <- as.numeric(gsub("NP", "0.001", base2_2$interstate_visitors)) * 1000
base2_2$LGA_FID <- as.integer(base2_2$LGA_FID)

# merge it to the LGA/Beach frame and replace the new international and interstate
check1 <- sum(base2_1$international_visitors)
check2 <- sum(base2_2$interstate_visitors)
print(paste("check1", check1))
print(paste("check2", check2))

INT_visit_data <- merge(INT_visit_data, base2_1, by = "LGA_FID", all.x = TRUE)
INT_visit_data <- merge(INT_visit_data, base2_2, by = "LGA_FID", all.x = TRUE)

rm(base2_1)
rm(base2_2)
gc()

# merge the driving distance from each LGA to each Beach
lga_dist <- read.csv("lga_distance.csv", encoding = "iso-8859-1")
colnames(lga_dist) <- c("LGA_FID", "LGA_Name", "Beach_FID", "Beach_Name", "Driving_d_LGA_Beach")

INT_visit_data <- merge(INT_visit_data, lga_dist[c("LGA_FID", "Beach_FID", "Driving_d_LGA_Beach")], by = c("LGA_FID", "Beach_FID"), all.x = TRUE)
rm(lga_dist)
gc()

INT_visit_data$Distance_m <- INT_visit_data$Driving_d_LGA_Beach
INT_visit_data <- INT_visit_data[, !(colnames(INT_visit_data) %in% c("Driving_d_LGA_Beach", "type_Modified_Used", "NEAR_RANK", "DIST_CBD_Km_Ali"))]

colnames(INT_visit_data) <- c("LGA_FID", "Beach_FID", "Distance_m", "LGA_Name", 
                              "Beach_Name", "Iconic", "Main", "National_Park", "Open_Coast", "Surf", "Bay",
                              "Length_B", "Width_B", "Changing room", "Patrolled_B",
                              "Restaurant", "Park", "total_visit_base1", "Beach_availability",
                              "international_visitors", "interstate_visitors")


## from pre-determined alpha, calculate the visitation from internaltional and interstate
### For BASE - international
# First, assign alpha as zero
df1 <- INT_visit_data
alpha1 <- read.csv("alpha1_international-step2.csv", encoding = "iso-8859-1")
df1 <- merge(df1, alpha1, by = "Beach_FID", all.x = TRUE, suffixes = c("", "_alpha1"))

rm(alpha1)
gc()

# Define utility calculation function
utility_calc_step2 <- function(frame, alpha_name, utility_name) {
  frame[[utility_name]] <- 0
  
  frame[[utility_name]] <- frame[[alpha_name]] +
    (frame[["Iconic"]] * 0.114) +
    (frame[["Surf"]] * 0.134) +
    (frame[["Bay"]] * 0.184) +
    ((frame[["Length_B"]] / 1000) * (-0.00549)) +
    (frame[["Width_B"]] * 0.0127) +
    (frame[["Changing room"]] * 0.404) +
    (frame[["Patrolled_B"]] * 0.379) +
    (frame[["Restaurant"]] * 0.259) +
    (frame[["Park"]] * 0.257) +
    ((frame[["Distance_m"]] / 1000) * (-0.0101))
  
  frame[[utility_name]] <- frame[[utility_name]] * frame[["Beach_availability"]]
  
  return(frame)
}

df1 <- utility_calc_step2(df1, "alpha_1", "utility_base")

# Calculate exponential of utility
df1[["exp_utility_base"]] <- exp(df1[["utility_base"]]) * df1[["Beach_availability"]]

# Group by LGA_FID and calculate the sum of exp_utility for each LGA
temp <- aggregate(df1[["exp_utility_base"]], by = list(df1[["LGA_FID"]]), sum)
colnames(temp) <- c("LGA_FID", "sum_exp_base")
df1 <- merge(df1, temp, by = "LGA_FID", all.x = TRUE)

# Calculate probability
df1[["prob_base"]] <- with(df1, exp_utility_base / sum_exp_base) * df1[["Beach_availability"]]

df1[["international_x_prob_base"]] <- with(df1, international_visitors * prob_base * Beach_availability)

temp <- aggregate(df1[["international_x_prob_base"]], by = list(df1[["Beach_FID"]]), sum)
colnames(temp) <- c("Beach_FID", "international_visit_beach_base")
df1 <- merge(df1, temp, by = "Beach_FID", all.x = TRUE)

diff_total_international_visitors <- sum(unique(df1[c("Beach_FID", "international_visit_beach_base")])$international_visit_beach_base) - sum(unique(INT_visit_data[c("LGA_FID", "international_visitors")])$international_visitors)
print(paste("Differences in the total international visitors between the sum of Beach and the sum of LGA: ", diff_total_international_visitors))

rm(temp)
gc()

### For BASE - interstate
# Define utility calculation function
utility_calc_step2 <- function(frame, alpha_name, utility_name) {
  frame[[utility_name]] <- 0
  
  frame[[utility_name]] <- frame[[alpha_name]] +
    (frame[["Iconic"]] * 0.114) +
    (frame[["Surf"]] * 0.134) +
    (frame[["Bay"]] * 0.184) +
    ((frame[["Length_B"]] / 1000) * (-0.00549)) +
    (frame[["Width_B"]] * 0.0127) +
    (frame[["Changing room"]] * 0.404) +
    (frame[["Patrolled_B"]] * 0.379) +
    (frame[["Restaurant"]] * 0.259) +
    (frame[["Park"]] * 0.257) +
    ((frame[["Distance_m"]] / 1000) * (-0.0101))
  
  frame[[utility_name]] <- frame[[utility_name]] * frame[["Beach_availability"]]
  
  return(frame)
}

df2 <- INT_visit_data
alpha2 <- read.csv("alpha2_interstate-step2.csv", encoding = "iso-8859-1")
df2 <- merge(df2, alpha2, by = "Beach_FID", all.x = TRUE)

rm(alpha2)
gc()

df2 <- utility_calc_step2(df2, "alpha_2", "utility_base")

# Calculate exponential of utility
df2[["exp_utility_base"]] <- exp(df2[["utility_base"]]) * df2[["Beach_availability"]]

# Group by LGA_FID and calculate the sum of exp_utility for each LGA
temp <- aggregate(df2[["exp_utility_base"]], by = list(df2[["LGA_FID"]]), sum)
colnames(temp) <- c("LGA_FID", "sum_exp_base")
df2 <- merge(df2, temp, by = "LGA_FID", all.x = TRUE)

# Calculate probability
df2[["prob_base"]] <- with(df2, exp_utility_base / sum_exp_base) * df2[["Beach_availability"]]

df2[["interstate_x_prob_base"]] <- with(df2, interstate_visitors * prob_base * Beach_availability)

temp <- aggregate(df2[["interstate_x_prob_base"]], by = list(df2[["Beach_FID"]]), sum)
colnames(temp) <- c("Beach_FID", "interstate_visit_beach_base")
df2 <- merge(df2, temp, by = "Beach_FID", all.x = TRUE)

diff_total_interstate_visitors <- sum(unique(df2[c("Beach_FID", "interstate_visit_beach_base")])$interstate_visit_beach_base) - sum(unique(INT_visit_data[c("LGA_FID", "interstate_visitors")])$interstate_visitors)
print(paste("Differences in the total interstate visitors between the sum of Beach and the sum of LGA: ", diff_total_interstate_visitors))

rm(temp)
gc()

### create a table to workout the domestic visitors 
final_table_base <- merge(
  unique(df1[c("Beach_FID", "Beach_Name", "total_visit_base1", "alpha_1", "international_visit_beach_base")]),
  unique(df2[c("Beach_FID", "Beach_Name", "alpha_2", "interstate_visit_beach_base")]),
  by = c("Beach_FID", "Beach_Name"),
  all.x = TRUE
)

print(nrow(final_table_base))

final_table_base$domestic_visit_beach <- final_table_base$total_visit_base1 - final_table_base$international_visit_beach_base - final_table_base$interstate_visit_beach_base

cat("should be zero here: ", sum(final_table_base$international_visit_beach_base) - check1)
cat("should be zero here: ", sum(final_table_base$interstate_visit_beach_base) - check2)
cat("total visit: ", sum(final_table_base$total_visit_base1))
cat("total international visit: ", sum(final_table_base$international_visit_beach_base))
cat("total interstate visit: ", sum(final_table_base$interstate_visit_beach_base))
cat("total domestic visit: ", sum(final_table_base$domestic_visit_beach))

cat("% of international visit: ", sum(final_table_base$international_visit_beach_base) / sum(final_table_base$total_visit_base1) * 100)
cat("% of interstate visit: ", sum(final_table_base$interstate_visit_beach_base) / sum(final_table_base$total_visit_base1) * 100)
cat("% of domestic visit: ", sum(final_table_base$domestic_visit_beach) / sum(final_table_base$total_visit_base1) * 100)

final_table_base$"% international" <- final_table_base$international_visit_beach_base / sum(final_table_base$international_visit_beach_base) * 100
final_table_base$"% interstate" <- final_table_base$interstate_visit_beach_base / sum(final_table_base$interstate_visit_beach_base) * 100
final_table_base$"% domestic" <- final_table_base$domestic_visit_beach / sum(final_table_base$domestic_visit_beach) * 100
final_table_base$"% total" <- final_table_base$total_visit_base1 / sum(final_table_base$total_visit_base1) * 100

final_table_base <- final_table_base[c(
  "Beach_FID", "Beach_Name", "total_visit_base1", "% total",
  "international_visit_beach_base", "% international",
  "interstate_visit_beach_base", "% interstate", "domestic_visit_beach",
  "% domestic"
)]


# step3
## calc domestic visitors for each postcode-beach pair - BASE
utility_calc_step3 <- function(frame, alpha_name, utility_name) {
  frame[[utility_name]] <- 0
  frame[[utility_name]] <- frame[[alpha_name]] +
    (frame$Iconic * 0.114) +
    (frame$Surf * 0.134) +
    (frame$Bay * 0.184) +
    ((frame$`Length_B` / 1000) * (-0.00549)) +
    (frame$`Width_B` * 0.0127) +
    (frame$`Changing room` * 0.404) +
    (frame$Patrolled_B * 0.379) +
    (frame$Restaurant * 0.259) +
    (frame$Park * 0.257) +
    ((frame$Distance_m / 1000) * (-0.0101))
  frame[[utility_name]] <- frame[[utility_name]] * frame$Beach_availability
  return(frame)
}

df3 <- df2[, c(
  "LGA_FID", "Beach_FID", "Distance_m", "LGA_Name", "Beach_Name", "Iconic",
  "Main", "National_Park", "Open_Coast", "Surf", "Bay", "Length_B",
  "Width_B", "Changing room", "Patrolled_B", "Restaurant", "Park", "Beach_availability"
)]

df3 <- merge(df3,
             final_table_base[c("Beach_FID", "domestic_visit_beach")],
             by = "Beach_FID",
             all.x = TRUE
)
print(nrow(df3))

LGA_data_new <- read.csv("LGA 2023 population and Seifa score.csv", encoding = "iso-8859-1")
colnames(LGA_data_new) <- c("LGA_Name", "LGA population 2023", "SEIFA")

LGA_data_new$LGA_Name <- recode(LGA_data_new$LGA_Name,
                                'Armidale' = 'Armidale Regional',
                                'Bathurst' = 'Bathurst Regional',
                                'Campbelltown (NSW)' = 'Campbelltown',
                                'Central Coast (NSW)' = 'Central Coast',
                                'Gundagai' = 'Cootamundra-Gundagai Regional',
                                'Dubbo' = 'Dubbo Regional',
                                'Greater Hume' = 'Greater Hume Shire',
                                'Mid-Western' = 'Mid-Western Regional',
                                'Queanbeyan-Palerang' = 'Queanbeyan-Palerang Regional',
                                'Snowy Monaro' = 'Snowy Monaro Regional',
                                'Sutherland' = 'Sutherland Shire',
                                'Tamworth' = 'Tamworth Regional',
                                'The Hills' = 'The Hills Shire',
                                'Upper Hunter' = 'Upper Hunter Shire',
                                'Upper Lachlan' = 'Upper Lachlan Shire',
                                'Warrumbungle' = 'Warrumbungle Shire')

LGA_data_new$SEIFA_weighted <- LGA_data_new$SEIFA / sum(LGA_data_new$SEIFA)

print(nrow(df3))
df3 <- merge(df3, LGA_data_new, by = "LGA_Name", all.x = TRUE)

rm(LGA_data_new)
gc()

print(nrow(df3))

alpha3 <- read.csv("alpha1_domestic-step3.csv", encoding = "iso-8859-1")

df3 <- merge(df3, alpha3, by = "Beach_FID", all.x = TRUE)
print(nrow(df3))
rm(alpha3)
gc()

df3 <- utility_calc_step3(df3, "alpha_3", "utility_base")

df3$exp_utility_base <- exp(df3$utility_base) * df3$Beach_availability

temp <- aggregate(df3$exp_utility_base, by = list(df3$LGA_FID), FUN = sum)
colnames(temp) <- c("LGA_FID", "sum_exp_base")
temp$sum_exp_base[temp$sum_exp_base == 0] <- 1
temp$log_sum_expbase <- log(temp$sum_exp_base)
temp$log_sum_expbase[temp$sum_exp_base == 0] <- 0

df3 <- merge(df3, temp, by = "LGA_FID", all.x = TRUE)

df3$prob_DO <- df3$exp_utility_base / df3$sum_exp_base * df3$Beach_availability
df3$monetrize_log_sum <- -df3$log_sum_expbase / ((1.19 * -0.0101)*2)

df3 <- df3 %>%
  rename(LGA_population_2023 = `LGA population 2023`)
df3$gravity_score <- with(df3, (LGA_population_2023 * SEIFA_weighted) / Distance_m)

temp <- aggregate(df3$gravity_score, by = list(df3$LGA_FID), FUN = sum)
colnames(temp) <- c("LGA_FID", "gravity_score_sum_LGA")
df3 <- merge(df3, temp, by = "LGA_FID", all.x = TRUE)

df3$po <- df3$gravity_score_sum_LGA / sum(df3$gravity_score, na.rm = TRUE)

df3 <- merge(df3, final_table_base[c("Beach_FID", "% domestic")], by = "Beach_FID", all.x = TRUE)
print(nrow(df3))

df3$pd <- df3$"% domestic" / 100

df3$pod1 <- with(df3, (prob_DO * po) / pd)
df3$pod1[df3$Beach_availability == 0] <- 0

temp <- aggregate(df3$pod1, by = list(df3$Beach_FID), FUN = sum, na.rm = TRUE)
colnames(temp) <- c("Beach_FID", "pod1_sum")
df3 <- merge(df3, temp, by = "Beach_FID", all.x = TRUE)
print(nrow(df3))

df3$"% pod1" <- df3$pod1 / df3$pod1_sum
df3$"% pod1"[df3$Beach_availability == 0] <- 0

df3$domestic_x_prob_base <- df3$domestic_visit_beach * df3$"% pod1"
temp <- aggregate(df3$domestic_x_prob_base, by = list(df3$LGA_FID), FUN = sum)
colnames(temp) <- c("LGA_FID", "Total_Domestic_visit_by_LGA")
df3 <- merge(df3, temp, by = "LGA_FID", all.x = TRUE)

print(sum(df3$domestic_x_prob_base, na.rm = TRUE))

df3$Weight <- df3$Total_Domestic_visit_by_LGA / df3$LGA_population_2023

df3$Monetized_Logsum_Wt_Base <- df3$monetrize_log_sum * df3$Weight

# selecting specific columns and renaming
temp <- BASE_data[c('Beach_FID','LGA_Name')]
names(temp) <- c('Beach_FID','Beach_LGA_Name')

# merging Beach LGA
df3 <- merge(df3, temp[c('Beach_FID','Beach_LGA_Name')], by='Beach_FID', all.x=TRUE)

# replacing values in 'Beach_LGA_Name' column
df3$Beach_LGA_Name[df3$Beach_LGA_Name == 'Sutherland'] <- 'Sutherland Shire'

# creating new columns 'withinLGA', 'within2hr' and 'outside2hr'
df3$withinLGA <- 0
df3$withinLGA[df3$LGA_Name == df3$Beach_LGA_Name] <- 1

df3$within2hr <- 0
df3$within2hr[(df3$Distance_m <= 120*1000) & (df3$withinLGA == 0)] <- 1

df3$outside2hr <- 0
df3$outside2hr[(df3$Distance_m > 120*1000) & (df3$withinLGA == 0)] <- 1

# further updating 'within2hr' and 'outside2hr' columns
df3$within2hr[(df3$Beach_LGA_Name %in% 'Mid-Coast' & df3$LGA_Name %in% c('Port Stephens','Dungog','Port Macquarie-Hastings'))] <- 1
df3$within2hr[(df3$Beach_LGA_Name %in% 'Eurobodalla' & df3$LGA_Name %in% c('Bega Valley','Shoalhaven','Queanbeyan-Palerang Regional'))] <- 1
df3$within2hr[(df3$Beach_LGA_Name %in% 'Bega Valley' & df3$LGA_Name %in% c('Eurobodalla','Snowy Monaro Regional'))] <- 1

df3$outside2hr[(df3$Beach_LGA_Name %in% 'Mid-Coast' & df3$LGA_Name %in% c('Port Stephens','Dungog','Port Macquarie-Hastings'))] <- 0
df3$outside2hr[(df3$Beach_LGA_Name %in% 'Eurobodalla' & df3$LGA_Name %in% c('Bega Valley','Shoalhaven','Queanbeyan-Palerang Regional'))] <- 0
df3$outside2hr[(df3$Beach_LGA_Name %in% 'Bega Valley' & df3$LGA_Name %in% c('Eurobodalla','Snowy Monaro Regional'))] <- 0

rm(temp)
gc()


# Define UI --------------------------------------------------------------------


ui <- navbarPage("Beach Evaluation Tool",
                 tabPanel("Introduction", 
                          h3(strong("NSW Beach values estimation tool (input for coastal management option cost-benefit analysis)")),
                          br(),
                          p("This tool provides beach values estimates for use in coastal management option cost-benefit analysis."),
                          p("The following values are estimated for each in the 29 coastal local government areas in NSW:"),
                          HTML("<ul><li>Consumer surplus</li><li>Producer surplus</li><li>Existence, bequest, option to use</li></ul>"),
                          br(),
                          p(em(strong("Resources:"))),
                          em("NSW beach values estimation tool user guide and technical appendicies"),
                          br(em("Coastal management options cost benefit analysis guidelines")),
                          p(),
                          HTML("<div style='font-size:12px;margin-top:20px;width:60%'>&#169 2023 State of NSW and Department of Planning and Environment.<br><br>With the exception of photographs, the State of NSW and Department of Planning and Environment (DPE) are pleased to allow this material to be reproduced in whole or in part for educational and non-commercial use, provided the meaning is unchanged and its source, publisher and authorship are acknowledged.<br><br>DPE has compiled this tool and user handbook in good faith, exercising all due care and attention. No representation is made about the accuracy, completeness or suitability of the information in the tool and publication which are intended for use by cost-benefit analysis (CBA) experts in undertaking NSW based coastal management options CBA. DPE shall not be liable for any damage which may occur to any person or organisation taking action or not on the basis of these publications. Readers should seek appropriate advice and refer to the user manual when applying the information to their specific needs.</div>")
                 ),
                 tabPanel("LGA CS & PS Values",
                          fluidPage(
                            tags$head(tags$script(
                              HTML(
                                "Shiny.addCustomMessageHandler('unbindDT', function(id) {
                                  var $table = $('#'+id).find('table');
                                  if($table.length > 0){
                                    Shiny.unbindAll($table.DataTable().table().node());
                                  }
                                })")
                            )),
                            includeCSS("www/dpie.css"),
                            includeCSS("www/jquery-ui.min.css"),
                            useShinyjs(),
                            tags$head(
                              tags$script(src = "www/jquery-ui.min.js"),
                              # tags$script("$(document).ready(function(){
                              #     $('.temp').tooltip();        
                              # });")
                              tags$style(type = 'text/css',
                                         '.modal-dialog { width: fit-content !important; }'
                              ),
                            ),
                            fluidRow(
                              h3(strong("LGA Boundary of Analysis - CS and PS")),
                              em("Beach values estimates inputs for use in cost-benefit analysis of coastal management options (annual, constant prices)"),
                              br(),
                              br(),
                              selectInput("lga", label = "Select LGA:", choices <- sort(setdiff(unique(data$LGA_Name), "Jervis Bay Territory"))),
                              HTML("<div style='float:right;text-align:right'>"),
                              downloadButton("downloadData", "Download"), 
                              HTML("</div>"),
                              br(),
                              br(),
                              dataTableOutput('lgacsps'),
                              br(),
                              HTML("<p style='font-size:10px'>September 2022 Prices (ABS CPI)<br>2022 LGA population forecast (Department of Planning and Environment)<br>Average 2019 to 2022 LGA tourist visitation data (Tourism Research Australia)")
                            )
                          )
                 ),
                 tabPanel("State CS & PS Values",
                          h3(strong("State Boundary of Analysis - CS and PS")),
                          h4(textOutput("selectedlga")),
                          HTML("<div style='float:right;text-align:right'>"),
                          downloadButton("downloadstateData", "Download"), 
                          HTML("</div>"),
                          br(),
                          br(),
                          dataTableOutput('statecsps'),
                          br(),
                          HTML("<p style='font-size:10px'>September 2022 Prices (ABS CPI)<br>2022 LGA population forecast (Department of Planning and Environment)<br>Average 2019 to 2022 LGA tourist visitation data (Tourism Research Australia)")
                          
                 ),
                 tabPanel("LGA & State EBO Values",
                          h3("Estimates existence, bequest/inheritence, option to user in the future value"),
                          h4(textOutput("selectedlga1")),
                          br(),
                          dataTableOutput("ebovalues"),
                          br(),
                          HTML("<p style='font-size:10px'>September 2022 Prices (ABS CPI)<br>2022 LGA population forecast (Department of Planning and Environment)<br>Average 2019 to 2022 LGA tourist visitation data (Tourism Research Australia)")
                          
                 )
                 
)



# Define server ----------------------------------------------------------------


server <- function(input, output,session) {
  
  # trying to write some JS to highlight changes made in the userinput section
  
  js<- "$('.temp').tooltip();
        $(document).on('shiny:inputchanged', function(event) {
         var getid=(event.name).substring(0,1);

          if(getid=='w'){
           var base=1;
           var scenario=5;
          }
          else{
           var base=2;
           var scenario=6;
          }
          table.rows().every(function(i, tab, row) {
            var $this = $(this.node());
            if(this.data()[base] !== parseInt($this[0].children[(scenario-1)].children[0].lastElementChild.value)){
             $this.find('td:nth-child('+scenario+') input').addClass('change');
            }
            else{
             $this.find('td:nth-child('+scenario+') input').removeClass('change');
            }
          });
        });"
  
  data_subset_modified <- reactive({
    
    temp_df <- data  %>% filter(LGA_Name == input$lga)
    # create the user inputs
    for (i in seq_len(nrow(temp_df))) {
      temp_df$Width_i[i] = as.character(numericInput(paste0("width",i),
                                                     label=NULL,
                                                     value=data$Width_S[temp_df$Beach_FID[i]],
                                                     width = "60px"))
      temp_df$Length_i[i] = as.character(numericInput(paste0("length",i),
                                                      label=NULL,
                                                      value=data$Length_S[temp_df$Beach_FID[i]],
                                                      width = "60px"))
      
      temp_df$Patrolled_i[i] = as.character(selectInput(paste0("patrolled", i),
                                                        label=NULL,
                                                        selectize=FALSE,
                                                        choices = c("Yes","No"),
                                                        selected = data$Patrolled_S[temp_df$Beach_FID[i]],
                                                        width = "60px"))
      
      temp_df$Close_i[i] = as.character(selectInput(paste0("close",i),
                                                    label=NULL,
                                                    selectize=FALSE,
                                                    choices = c("Yes","No"),
                                                    selected=data$Close_S[temp_df$Beach_FID[i]],
                                                    width = "60px"))
      
      temp_df$csb[i] = as.character(disabled(textInput(paste0("csb",i),
                                                       label=NULL,
                                                       value="$ 0",
                                                       width="100%")))
      
      temp_df$css[i] = as.character(disabled(textInput(paste0("css",i),
                                                       label=NULL,
                                                       value="$ 0",
                                                       width="100%")))
      temp_df$csc[i] = as.character(disabled(textInput(paste0("csc",i),
                                                       label=NULL,
                                                       value="$ 0",
                                                       width="100%")))
      
      temp_df$psb[i] = as.character(disabled(textInput(paste0("psb",i),
                                                       label=NULL,
                                                       value="$ 0",
                                                       width="100%")))
      temp_df$pss[i] = as.character(disabled(textInput(paste0("pss",i),
                                                       label=NULL,
                                                       value="$ 0",
                                                       width="100%")))
      temp_df$psc[i] = as.character(disabled(textInput(paste0("psc",i),
                                                       label=NULL,
                                                       value="$ 0",
                                                       width="100%")))
      
      
      temp_df$eb[i] = as.character(disabled(textInput(paste0("eb",i),
                                                       label=NULL,
                                                       value="$ 0",
                                                       width="100%")))
      temp_df$es[i] = as.character(disabled(textInput(paste0("es",i),
                                                       label=NULL,
                                                       value="$ 0",
                                                       width="100%")))
      temp_df$ec[i] = as.character(disabled(textInput(paste0("ec",i),
                                                       label=NULL,
                                                       value="$ 0",
                                                       width="100%")))
    }
    
    
    return(temp_df)
    
  })
  
  
  output$lgacsps <- DT::renderDataTable({
    # Render the table
    datatable(
      data_subset_modified()[,c(3,4,5,6,11,12,13,14,15,16,17,18,19,20)],
      class = 'cell-border stripe',
      rownames = FALSE,
      escape = FALSE,
      container = table_frame1(),
      callback = JS(js),
      selection = "none",
      options = list(
        searching = FALSE,
        preDrawCallback = JS('function() {
                           Shiny.unbindAll(this.api().table().node());
                             }'),
        drawCallback = JS('function() {
                        Shiny.bindAll(this.api().table().node()); } ') ,
        dom = 'Bftrip',
        paging = FALSE,
        info = FALSE,
        columnDefs = list(list(
          className = 'dt-center', targets = 1:3),list(className='dt-center temp', targets= 4:13), list(width='250px', targets=c(0)), 
          list(width="60px", targets=c(1,2,3)),
          list(width="60px", targets=c(4,5,6,7))
        ),
        rownames = FALSE,
        ordering = FALSE
      )
    ) 
  })
  

  #render table for state cs/ps
  output$statecsps <- DT::renderDataTable({
    datatable(
     data_subset_modified()[,c(3,4,5,6,7,8,9,10,15,16,17,18,19,20)],
      class = 'cell-border stripe',
      rownames = FALSE,
      escape = FALSE,
      container = table_frame2(),
      # callback = JS(js),
      selection = "none",
      options = list(
        searching = FALSE,
        preDrawCallback = JS('function() {
                           Shiny.unbindAll(this.api().table().node());
                             }'),
        drawCallback = JS('function() {
                        Shiny.bindAll(this.api().table().node()); } ') ,
        dom = 'Bftrip',
        paging = FALSE,
        info = FALSE,
        columnDefs = list(list(
          className = 'dt-center', targets = 1:3),list(className='dt-center temp', targets= 4:13), list(width='200px', targets=c(0)), 
          list(width="60px", targets=c(1,2,3)),
          list(width="60px", targets=c(4,5,6,7))
        ),
        rownames = FALSE,
        ordering = FALSE
      )
    ) 
  })
  
  #render table for LGA per visit CS/PS
  output$visitation_output_lga <- DT::renderDataTable({
    # Render the table
    datatable(
      data_subset_modified()[,c(3,4,5,6,11,12,13,14,15,16,17,18,19,20)],
      class = 'cell-border stripe',
      rownames = FALSE,
      escape = FALSE,
      container = table_visitation_lga(),
      callback = JS(js),
      selection = "none",
      options = list(
        searching = FALSE,
        preDrawCallback = JS('function() {
                           Shiny.unbindAll(this.api().table().node());
                             }'),
        drawCallback = JS('function() {
                        Shiny.bindAll(this.api().table().node()); } ') ,
        dom = 'Bftrip',
        paging = FALSE,
        info = FALSE,
        columnDefs = list(list(
          className = 'dt-center', targets = 1:3), list(width='60px', targets=c(0)),list(width="60px", targets=1:16)
        ),
        rownames = FALSE,
        ordering = FALSE
      )
    ) 
  })
  

  
  #render table for State per visit CS/PS
  output$visitation_output_state <- DT::renderDataTable({
    # Render the table
    datatable(
      data_subset_modified()[,c(3,4,5,6,11,12,13,14,15,16,17,18,19,20)],
      class = 'cell-border stripe',
      rownames = FALSE,
      escape = FALSE,
      container = table_visitation_state(),
      callback = JS(js),
      selection = "none",
      options = list(
        searching = FALSE,
        preDrawCallback = JS('function() {
                           Shiny.unbindAll(this.api().table().node());
                             }'),
        drawCallback = JS('function() {
                        Shiny.bindAll(this.api().table().node()); } ') ,
        dom = 'Bftrip',
        paging = FALSE,
        info = FALSE,
        columnDefs = list(list(
          className = 'dt-center', targets = 1:3), list(width='250px', targets=c(0)),list(width="60px", targets=1:16)
        ),
        rownames = FALSE,
        ordering = FALSE
      )
    ) 
  })
  
  
  #create modal dialog for LGA visitation output 
  visitationModal_lga <- function(failed = FALSE) {
    modalDialog(
      h3(strong('LGA Boundary of Analysis')),
      h4(strong(paste("Underlying visitation data for ", input$lga))),
      br(),     
      HTML("<div style='float:right;text-align:right'>"),
      downloadButton("downloadvisitationData_lga", "Download"), 
      HTML("</div>"),
      br(),
      br(),
      dataTableOutput("visitation_output_lga"),
      if (failed)
        div(tags$b("Table error.", style = "color: red;")),
      
      footer = tagList(
        modalButton("Close"),
      )
    )
  }
  
  #create modal dialog for LGA chart output
  lgachartModal <- function(failed = FALSE) {
    modalDialog(
      h4(strong(paste("LGA Net impacts for ", input$lga))),
      br(),
      br(),
      plotlyOutput("lgawaterfall"),
      if (failed)
        div(tags$b("Table error.", style = "color: red;")),
      
      footer = tagList(
        modalButton("Close"),
      )
    )
  }
  
  #create modal dialog for State visitation output 
  visitationModal_state <- function(failed = FALSE) {
    modalDialog(
      h3(strong('State Boundary of Analysis')),
      h4(strong(paste("Underlying visitation data for ", input$lga))),
      br(),     
      HTML("<div style='float:right;text-align:right'>"),
      downloadButton("downloadvisitationData_state", "Download"), 
      HTML("</div>"),
      br(),
      br(),
      dataTableOutput("visitation_output_state"),
      if (failed)
        div(tags$b("Table error.", style = "color: red;")),
      
      footer = tagList(
        modalButton("Close"),
      )
    )
  }
  
  #create modal dialog for LGA chart output
  statechartModal <- function(failed = FALSE) {
    modalDialog(
      h4(strong(paste("State Net impacts for ", input$lga))),
      br(),
      br(),
      plotlyOutput("lgawaterfall"),
      if (failed)
        div(tags$b("Table error.", style = "color: red;")),
      
      footer = tagList(
        modalButton("Close"),
      )
    )
  }
  
  output$ebovalues<-DT::renderDataTable({
    datatable(
      data_subset_modified()[,c(3,4,5,6,11,12,13,14,15,16,17,18,19,20)],
      class = 'cell-border stripe',
      rownames = FALSE,
      escape = FALSE,
      container = ebo(),
      callback = JS(js),
      selection = "none",
      options = list(
        searching = FALSE,
        preDrawCallback = JS('function() {
                           Shiny.unbindAll(this.api().table().node());
                             }'),
        drawCallback = JS('function() {
                        Shiny.bindAll(this.api().table().node()); } ') ,
        dom = 'Bftrip',
        paging = FALSE,
        info = FALSE,
        columnDefs = list(list(
          className = 'dt-center', targets = 1:3),list(className='dt-center temp', targets= 4:13), list(width='250px', targets=c(0)),
          list(width="60px", targets=c(1,2,3)),
          list(width="60px", targets=c(4,5,6,7))
        ),
        rownames = FALSE,
        ordering = FALSE
      )
    )
  })
  
  #open the visitation modal dialog when clicked
  observeEvent(input$lga_visitation,{
    showModal(visitationModal_lga())
    
  })
  
  #open the lga chart modal dialog when clicked
  observeEvent(input$lgachart,{
    showModal(lgachartModal())
    
  })
  
  #open the visitation modal dialog when clicked
  observeEvent(input$state_visitation,{
    showModal(visitationModal_state())
    
  })
  
  #open the lga chart modal dialog when clicked
  observeEvent(input$statechart,{
    showModal(statechartModal())
    
  })
  
  #Example waterfall chart of LGA Net Impacts
  output$lgawaterfall <- renderPlotly({
    #dummy waterfall chart info
    x= list("CS LGA", "PS LGA", "EBO LGA", "Total LGA")
    measure= c("relative","relative","relative","total")
    text=c("-$2,408,726","-$232,000","-$49,650","-$2,690,376")
    y=c(-2408726,-232000,-49650,0)
    chart = data.frame(x=factor(x,levels=x),measure,text,y)
    
    fig <- plot_ly(
      chart, name = "20", type = "waterfall", measure = ~measure,
      x = ~x, textposition = "outside", y= ~y, text =~text, decreasing=list(marker=list(color="#002664")), totals=list(marker=list(color="#495054")), width=700, height=400,
      connector = list(line = list(color= "rgb(63, 63, 63)")))
    fig <- fig %>%
      layout(title = "Net LGA boundary of analysis impacts estimates",
             xaxis = list(title = ""),
             yaxis = list(title = "", tickformat="$,"),
             autosize = FALSE,
             showlegend = FALSE,
             font=plotfont
      )
    fig
  })
  
  
  # Using lapply for observers
  # split up the lapplys and removed from the overarching observe() as it initialises a new observer each time
  
  lapply(seq_len(nrow(isolate(data_subset_modified()))), function(i) {
    widthId <- paste0("width", i)
    observeEvent(input[[widthId]], {
      print(paste("Index:", i, "Width value:", input[[widthId]],
                  "Beach_FID:", data_subset_modified()$Beach_FID[i]))
      
      #if user clears the input, revert back to base value
      if(is.na(input[[widthId]])){
        updateNumericInput(session,widthId,value=data_subset_modified()$Width_B[i])
      }
      data[data$Beach_FID == data_subset_modified()$Beach_FID[i], "Width_S"] <<- input[[widthId]]
    }, ignoreInit = TRUE)
  })
  
  lapply(seq_len(nrow(isolate(data_subset_modified()))), function(i) {
    lengthId <- paste0("length", i)
    observeEvent(input[[lengthId]], {
      
      
      print(paste("Index:", i, "Length value:", input[[lengthId]],
                  "Beach_FID:", data_subset_modified()$Beach_FID[i]))
      
      #if user clears the input, revert back to base value
      if(is.na(input[[lengthId]])){
        updateNumericInput(session,lengthId,value=data_subset_modified()$Length_B[i])
      }
      data[data$Beach_FID == data_subset_modified()$Beach_FID[i], "Length_S"] <<- input[[lengthId]]
      
    }, ignoreInit = TRUE)
  })
  
  lapply(seq_len(nrow(isolate(data_subset_modified()))), function(i) {
    patrolledId <- paste0("patrolled", i)
    observeEvent(input[[patrolledId]], {
      
      
      print(paste("Index:", i, "Patrolled value:", input[[patrolledId]],
                  "Beach_FID:", data_subset_modified()$Beach_FID[i]))
      
      data[data$Beach_FID == data_subset_modified()$Beach_FID[i], "Patrolled_S"] <<- input[[patrolledId]]
      
    }, ignoreInit = TRUE)
  })
  
  lapply(seq_len(nrow(isolate(data_subset_modified()))), function(i) {
    closeId <- paste0("close", i)
    observeEvent(input[[closeId]], {
      
      
      print(paste("Index:", i, "Closed value:", input[[closeId]],
                  "Beach_FID:", data_subset_modified()$Beach_FID[i]))
      
      data[data$Beach_FID == data_subset_modified()$Beach_FID[i], "Close_S"] <<- input[[closeId]]
      
    }, ignoreInit = TRUE)
  })
  
  
  #unbind the dataTables on input change otherwise interactivity is lost
  observeEvent(input$lga,{
    session$sendCustomMessage("unbindDT", "lgacsps")
    output$selectedlga<-renderText(paste("Selected LGA: ",input$lga))
    output$selectedlga1<-renderText(paste("Selected LGA: ",input$lga))
  })
  
  
  observeEvent(input$reset, {
    
    for (i in seq_len(nrow(data_subset_modified()))) {
      updateNumericInput(session, paste0("width", i), value = data_subset_modified()$Width_B[i])
      updateNumericInput(session, paste0("length", i), value = data_subset_modified()$Length_B[i])
      updateSelectInput(session, paste0("patrolled", i), choices = c("Yes", "No"), selected = data_subset_modified()$Patrolled_B[i])
      updateSelectInput(session, paste0("close", i), choices = c("Yes", "No"), selected = "No")
      
      # updateTextInput(session, paste0("csb", i), paste("$", value = prettyNum(1, big.mark=",", scientific=FALSE)))
      updateTextInput(session, paste0("csb", i), value = '$ 0')
      updateTextInput(session, paste0("css", i), value = '$ 0')
      updateTextInput(session, paste0("csc", i), value = '$ 0')
      
      updateTextInput(session, paste0("psb", i), value = '$ 0')
      updateTextInput(session, paste0("pss", i), value = '$ 0')
      updateTextInput(session, paste0("psc", i), value = '$ 0')
    }
  })
  
  #create download button for LGA CS/PS data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write.xlsx(data, file, rowNames = FALSE)
    } 
  )   
  
  #create download button for visitation data
  output$downloadvisitationData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write.xlsx(data, file, rowNames = FALSE)
    } 
  )   
  
  #create download button for State CS/PS data
  output$downloadstateData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write.xlsx(data, file, rowNames = FALSE)
    } 
  )   
  
  
  # Calculation function
  observeEvent(input$calculate, {
    
    # Initialize the progress bar
    progress <- shiny::Progress$new()
    
    # Start the progress bar with a message
    progress$set(message = "Processing calculation...", value = 0)
    
    df1_s <- df1
    df1_s <- select(df1_s, -c(Patrolled_B, Length_B, Width_B, Beach_availability))
    
    df2_s <- df2
    df2_s <- select(df2_s, -c(Patrolled_B, Length_B, Width_B, Beach_availability))
    
    df3_s <- df3
    df3_s <- select(df3_s, -c(Patrolled_B, Length_B, Width_B, Beach_availability))
    
    df3_s_1 <- df3
    df3_s_1 <- select(df3_s_1, -c(Patrolled_B, Length_B, Width_B, Beach_availability))
    
    ## adjust international for scenario
    df1_s <- merge(df1_s, data[, c("Beach_FID", "Patrolled_S", "Length_S", "Width_S", "Close_S")], by = "Beach_FID", all.x = TRUE)
    df1_s$Patrolled_S <- ifelse(df1_s$Patrolled_S == "Yes", 1, 0)
    df1_s$Beach_availability <- ifelse(df1_s$Close_S == "Yes", 0, 1)
    
    # Define utility calculation function
    utility_calc_step2_s <- function(frame, alpha_name, utility_name) {
      frame[[utility_name]] <- 0
      
      frame[[utility_name]] <- frame[[alpha_name]] +
        (frame[["Iconic"]] * 0.114) +
        (frame[["Surf"]] * 0.134) +
        (frame[["Bay"]] * 0.184) +
        ((frame[["Length_S"]] / 1000) * (-0.00549)) +
        (frame[["Width_S"]] * 0.0127) +
        (frame[["Changing room"]] * 0.404) +
        (frame[["Patrolled_S"]] * 0.379) +
        (frame[["Restaurant"]] * 0.259) +
        (frame[["Park"]] * 0.257) +
        ((frame[["Distance_m"]] / 1000) * (-0.0101))
      
      frame[[utility_name]] <- frame[[utility_name]] * frame[["Beach_availability"]]
      
      return(frame)
    }
    
    df1_s <- utility_calc_step2_s(df1_s, 'alpha_1', 'utility_scenario')
    
    # Simply group by LGA_FID then calculate the sum of exp_utility for each LGA
    temp <- unique(df1_s[c('LGA_FID', 'sum_exp_base')])
    temp['deno'] <- 1 - (temp$sum_exp_base + 1)**(-1)
    
    # Simply calculate the exponential of utility
    df1_s['exp_utility_scenario'] <- exp(df1_s$utility_scenario) * df1_s$Beach_availability
    
    temp_scenario <- df1_s %>%
      group_by(LGA_FID) %>%
      summarise(sum_exp_scenario = sum(exp_utility_scenario)) %>%
      ungroup()
    temp_scenario['numer'] <- 1 - (temp_scenario$sum_exp_scenario + 1)**(-1)
    
    temp <- merge(temp, temp_scenario, by = "LGA_FID", all.x = TRUE)
    
    # Remove duplicates based on LGA_FID and international_visitors, and merge with temp
    temp <- merge(temp,
                  unique(df1_s[c('LGA_FID', 'international_visitors')]),
                  by = "LGA_FID",
                  all.x = TRUE)
    
    temp$T_O_scenario <- with(temp, (numer / deno) * international_visitors)
    
    df1_s <- merge(df1_s, temp[, c('LGA_FID', 'T_O_scenario', 'sum_exp_scenario')], by = 'LGA_FID', all.x = TRUE)
    
    df1_s$prob_scenario <- with(df1_s, (exp_utility_scenario / sum_exp_scenario) * Beach_availability)
    
    df1_s$international_x_prob_scenario <- with(df1_s, T_O_scenario * prob_scenario * Beach_availability)
    
    temp <- aggregate(df1_s$international_x_prob_scenario, by = list(df1_s$Beach_FID), sum)
    colnames(temp) <- c('Beach_FID', 'international_visit_beach_scenario')
    df1_s <- merge(df1_s, temp, by = 'Beach_FID', all.x = TRUE)
    
    cat("Total number of international visitors in base case:", format(sum(df1_s$international_x_prob_base), big.mark = ",", scientific = FALSE), "\n")
    cat("Total number of international visitors in scenario:", format(sum(df1_s$international_x_prob_scenario), big.mark = ",", scientific = FALSE), "\n")
    cat("Total number of international visitors drop:", format(sum(df1_s$international_x_prob_base) - sum(df1_s$international_x_prob_scenario), big.mark = ",", scientific = FALSE), "\n")
    
    
    progress$inc(2/10)
    
    rm(temp)
    rm(temp_scenario)
    gc()
    
    ## adjust interstate for scenario
    df2_s <- merge(df2_s, data[, c("Beach_FID", "Patrolled_S", "Length_S", "Width_S", "Close_S")], by = "Beach_FID", all.x = TRUE)
    df2_s$Patrolled_S <- ifelse(df2_s$Patrolled_S == "Yes", 1, 0)
    df2_s$Beach_availability <- ifelse(df2_s$Close_S == "Yes", 0, 1)
    
    df2_s <- utility_calc_step2_s(df2_s, 'alpha_2', 'utility_scenario')
    
    # Simply group by LGA_FID then calculate the sum of exp_utility for each LGA
    temp <- unique(df2_s[c('LGA_FID', 'sum_exp_base')])
    temp['deno'] <- 1 - (temp$sum_exp_base + 1)**(-1)
    
    # Simply calculate the exponential of utility
    df2_s['exp_utility_scenario'] <- exp(df2_s$utility_scenario) * df2_s$Beach_availability
    
    temp_scenario <- df2_s %>%
      group_by(LGA_FID) %>%
      summarise(sum_exp_scenario = sum(exp_utility_scenario)) %>%
      ungroup()
    temp_scenario['numer'] <- 1 - (temp_scenario$sum_exp_scenario + 1)**(-1)
    
    temp <- merge(temp, temp_scenario, by = "LGA_FID", all.x = TRUE)
    
    # Remove duplicates based on LGA_FID and interstate_visitors, and merge with temp
    temp <- merge(temp,
                  unique(df2_s[c('LGA_FID', 'interstate_visitors')]),
                  by = "LGA_FID",
                  all.x = TRUE)
    
    temp$T_O_scenario <- (temp$numer / temp$deno) * temp$interstate_visitors
    
    df2_s <- merge(df2_s, temp[, c('LGA_FID', 'T_O_scenario', 'sum_exp_scenario')], by = 'LGA_FID', all.x = TRUE)
    
    df2_s$prob_scenario <- with(df2_s, (exp_utility_scenario / sum_exp_scenario) * Beach_availability)
    
    df2_s$interstate_x_prob_scenario <- with(df2_s, T_O_scenario * prob_scenario * Beach_availability)
    
    temp <- aggregate(df2_s$interstate_x_prob_scenario, by = list(df2_s$Beach_FID), sum)
    colnames(temp) <- c('Beach_FID', 'interstate_visit_beach_scenario')
    df2_s <- merge(df2_s, temp, by = 'Beach_FID', all.x = TRUE)
    
    cat("Total number of interstate visitors in base case:", format(sum(df2_s$interstate_x_prob_base), big.mark = ",", scientific = FALSE), "\n")
    cat("Total number of interstate visitors in scenario:", format(sum(df2_s$interstate_x_prob_scenario), big.mark = ",", scientific = FALSE), "\n")
    cat("Total number of interstate visitors drop:", format(sum(df2_s$interstate_x_prob_base) - sum(df2_s$interstate_x_prob_scenario), big.mark = ",", scientific = FALSE), "\n")
    
    
    progress$inc(4/10)
    
    rm(temp)
    rm(temp_scenario)
    gc()
    
    ## adjust total visitors for scenario
    df3_s <- merge(df3_s, data[, c("Beach_FID", "Patrolled_S", "Length_S", "Width_S", "Close_S")], by = "Beach_FID", all.x = TRUE)
    df3_s$Patrolled_S <- ifelse(df3_s$Patrolled_S == "Yes", 1, 0)
    df3_s$Beach_availability <- ifelse(df3_s$Close_S == "Yes", 0, 1)
    
    utility_calc_step3_s <- function(frame, alpha_name, utility_name) {
      frame[[utility_name]] <- 0
      
      frame[[utility_name]] <- frame[[alpha_name]] +
        (frame[["Iconic"]] * 0.114) +
        (frame[["Surf"]] * 0.134) +
        (frame[["Bay"]] * 0.184) +
        ((frame[["Length_S"]] / 1000) * (-0.00549)) +
        (frame[["Width_S"]] * 0.0127) +
        (frame[["Changing room"]] * 0.404) +
        (frame[["Patrolled_S"]] * 0.379) +
        (frame[["Restaurant"]] * 0.259) +
        (frame[["Park"]] * 0.257) +
        ((frame[["Distance_m"]] / 1000) * (-0.0101))
      
      frame[[utility_name]] <- frame[[utility_name]] * frame[["Beach_availability"]]
      
      return(frame)
    }
    
    df3_s <- utility_calc_step3_s(df3_s, 'alpha_3', 'utility_scenario')
    
    # Simply group by LGA_FID then calculate the sum of exp_utility for each LGA
    temp <- unique(df3_s[c('LGA_FID', 'sum_exp_base')])
    temp$deno <- 1 - (temp$sum_exp_base + 1)**(-1)
    
    # Simply calculate the exponential of utility
    df3_s$exp_utility_scenario <- exp(df3_s$utility_scenario) * df3_s$Beach_availability
    
    temp_scenario <- df3_s %>%
      group_by(LGA_FID) %>%
      summarise(sum_exp_scenario = sum(exp_utility_scenario)) %>%
      ungroup()
    temp_scenario$numer <- 1 - (temp_scenario$sum_exp_scenario + 1)**(-1)
    
    temp <- merge(temp, temp_scenario, by = "LGA_FID", all.x = TRUE)
    
    temp_total_domestic <- unique(df3_s[c('LGA_FID','Total_Domestic_visit_by_LGA')])
    
    temp <- merge(temp, temp_total_domestic, by = "LGA_FID", all.x = TRUE)
    
    temp$T_O_scenario <- (temp$numer / temp$deno) * temp$Total_Domestic_visit_by_LGA
    
    df3_s <- merge(df3_s, temp[, c('LGA_FID', 'T_O_scenario', 'sum_exp_scenario')], by = 'LGA_FID', all.x = TRUE)
    
    df3_s$prob_scenario <- with(df3_s, (exp_utility_scenario / sum_exp_scenario) * Beach_availability)
    
    df3_s$domestic_x_prob_scenario <- with(df3_s, T_O_scenario * prob_scenario * Beach_availability)
    
    temp_out <- aggregate(df3_s['domestic_x_prob_scenario'], by = list(df3_s$Beach_FID), sum)
    names(temp_out) <- c('Beach_FID', 'domestic_visitors_scenario')
    
    temp_out$'% domestic_scenario' <- (temp_out$domestic_visitors_scenario / sum(temp_out$domestic_visitors_scenario)) * 100
    df3_s <- merge(df3_s, temp_out, by = c('Beach_FID'), all.x = TRUE)
    
    cat("Total number of domestic visitors in base case:", format(sum(df3_s$domestic_x_prob_base), big.mark = ",", scientific = FALSE), "\n")
    cat("Total number of domestic visitors in scenario:", format(sum(df3_s$domestic_x_prob_scenario), big.mark = ",", scientific = FALSE), "\n")
    cat("Total number of domestic visitors drop:", format(sum(df3_s$domestic_x_prob_base) - sum(df3_s$domestic_x_prob_scenario), big.mark = ",", scientific = FALSE), "\n")
    
    
    progress$inc(6/10)
    
    rm(temp)
    rm(temp_scenario)
    gc()
    
    # calc the consumer surplus for scenario
    df3_s_1 <- merge(df3_s_1, data[, c("Beach_FID", "Patrolled_S", "Length_S", "Width_S", "Close_S")], by = "Beach_FID", all.x = TRUE)
    df3_s_1$Patrolled_S <- ifelse(df3_s_1$Patrolled_S == "Yes", 1, 0)
    df3_s_1$Beach_availability <- ifelse(df3_s_1$Close_S == "Yes", 0, 1)
    
    print(paste("++++++++++++> Beach_LGA_Name in df3_s_1: ", "Beach_LGA_Name" %in% names(df3_s_1)))
    
    cat(nrow(df3_s_1), "\n")
    df3_s_1 <- merge(df3_s_1, temp_out, by = 'Beach_FID', all.x = TRUE)
    cat(nrow(df3_s_1), "\n")
    
    df3_s_1[['utility_scenario']] <- 0
    df3_s_1[['utility_scenario']] <- df3_s_1[['alpha_3']] +
      (df3_s_1[["Iconic"]] * 0.114) +
      (df3_s_1[["Surf"]] * 0.134) +
      (df3_s_1[["Bay"]] * 0.184) +
      ((df3_s_1[["Length_S"]] / 1000) * (-0.00549)) +
      (df3_s_1[["Width_S"]] * 0.0127) +
      (df3_s_1[["Changing room"]] * 0.404) +
      (df3_s_1[["Patrolled_S"]] * 0.379) +
      (df3_s_1[["Restaurant"]] * 0.259) +
      (df3_s_1[["Park"]] * 0.257) +
      ((df3_s_1[["Distance_m"]] / 1000) * (-0.0101))
    df3_s_1[['utility_scenario']] <- df3_s_1[['utility_scenario']] * df3_s_1[["Beach_availability"]]
    
    df3_s_1$exp_utility_scenario <- exp(df3_s_1$utility_scenario) * df3_s_1$Beach_availability
    
    temp <- df3_s_1 %>%
      group_by(LGA_FID) %>%
      summarise(sum_exp_scenario = sum(exp_utility_scenario))
    
    # If the sum is zero, set it to 1
    temp$sum_exp_scenario[temp$sum_exp_scenario == 0] <- 1
    
    # Take the logarithm of the sum, set it to 0 if it is already 0
    temp$log_sum_expscenario <- log(temp$sum_exp_scenario)
    temp$log_sum_expscenario[temp$sum_exp_scenario == 0] <- 0
    
    # Merge it back to df3_s_1
    df3_s_1 <- left_join(df3_s_1, temp, by = "LGA_FID")
    
    df3_s_1$prob_DO_scenario <- with(df3_s_1, exp_utility_scenario / sum_exp_scenario) * df3_s_1$Beach_availability
    df3_s_1$monetrize_log_sum <- (df3_s_1$log_sum_expscenario * (-1)) / (1.19 * (-0.0101) * 2)
    
    df3_s_1$pd <- df3_s_1$'% domestic_scenario' / 100
    
    df3_s_1$pod_scenario <- with(df3_s_1, (prob_DO_scenario * po) / pd)
    df3_s_1[df3_s_1$Beach_availability == 0, 'pod_scenario'] <- 0
    
    temp <- aggregate(df3_s_1$pod_scenario, by = list(df3_s_1$Beach_FID), sum)
    colnames(temp) <- c('Beach_FID', 'pod_scenario_sum')
    df3_s_1 <- merge(df3_s_1, temp, by = 'Beach_FID', all.x = TRUE)
    
    df3_s_1$'% pod_scenario' <- df3_s_1$pod_scenario / df3_s_1$pod_scenario_sum
    df3_s_1[df3_s_1$Beach_availability == 0, '% pod_scenario'] <- 0
    
    df3_s_1$domestic_x_prob_scenario <- df3_s_1$domestic_visitors_scenario * df3_s_1$'% pod_scenario'
    
    cat(sum(df3_s_1$domestic_x_prob_scenario), "\n")
    
    df3_s_1$Weight <- df3_s_1$Total_Domestic_visit_by_LGA / df3_s_1$LGA_population_2023
    
    df3_s_1$Monetized_Logsum_Wt_scenario <- df3_s_1$monetrize_log_sum * df3_s_1$Weight
    
    df3_s_1$Total_CS_Base <- df3_s_1$domestic_x_prob_base * df3_s_1$Monetized_Logsum_Wt_scenario
    df3_s_1$Total_CS_Scenario <- df3_s_1$domestic_x_prob_scenario * df3_s_1$Monetized_Logsum_Wt_scenario
    df3_s_1$CS_Delta <- df3_s_1$Total_CS_Scenario - df3_s_1$Total_CS_Base
    
    
    rm(temp)
    gc()
    
    progress$inc(8/10)
    progress$close()
    
    # create LGA_CS - Filter rows
    LGA_CS <- df3_s_1[df3_s_1$LGA_Name==input$lga & df3_s_1$Beach_FID %in% as.list(sort(unique(BASE_data[BASE_data$LGA_Name == input$lga, "Beach_FID"]))),]
    
    # Select columns
    LGA_CS <- LGA_CS[, c('Beach_FID', 'Total_CS_Base', 'Total_CS_Scenario', 'CS_Delta')]
    
    # Sort dataframe by 'Beach_FID'
    LGA_CS <- LGA_CS[order(LGA_CS$Beach_FID), ]
    
    #create Producer surplus for LGA boundary
    
    LGA_PS <- final_table_base[final_table_base$Beach_FID %in% unique(BASE_data[BASE_data$LGA_Name == input$lga, 'Beach_FID']),]
    
    temp <- df3 %>%
      filter(within2hr == 1) %>%
      group_by(Beach_FID) %>%
      summarise(domestic_x_prob_base = sum(domestic_x_prob_base)) %>%
      rename(within2hr_visitors_base = domestic_x_prob_base)
    LGA_PS <- merge(LGA_PS, temp, by = 'Beach_FID', all.x = TRUE)
    LGA_PS$within2hr_visitors_base[is.na(LGA_PS$within2hr_visitors_base)] <- 0
    
    temp <- df3 %>%
      filter(outside2hr == 1) %>%
      group_by(Beach_FID) %>%
      summarise(domestic_x_prob_base = sum(domestic_x_prob_base)) %>%
      rename(outside2hr_visitors_base = domestic_x_prob_base)
    LGA_PS <- merge(LGA_PS, temp, by = 'Beach_FID', all.x = TRUE)
    LGA_PS$outside2hr_visitors_base[is.na(LGA_PS$outside2hr_visitors_base)] <- 0
    
    # expenditure for PS:
    ps_expense <- read.csv('Producer Surplus.csv', fileEncoding = 'ISO-8859-1')
    names(ps_expense) <- c('Beach_LGA_Name','international_expense','day_expense','overnight_expense')
    ps_expense$Beach_LGA_Name <- sub('Bega', 'Bega Valley', ps_expense$Beach_LGA_Name)
    ps_expense$Beach_LGA_Name <- sub('Clarence', 'Clarence Valley', ps_expense$Beach_LGA_Name)
    ps_expense$Beach_LGA_Name <- sub('Mid Coast', 'Mid-Coast', ps_expense$Beach_LGA_Name)
    ps_expense$Beach_LGA_Name <- sub('Nambucca', 'Nambucca Valley', ps_expense$Beach_LGA_Name)
    ps_expense$Beach_LGA_Name <- sub('Port Macquarie-Hasting', 'Port Macquarie-Hastings', ps_expense$Beach_LGA_Name)
    ps_expense$Beach_LGA_Name <- sub('Sutherland', 'Sutherland Shire', ps_expense$Beach_LGA_Name)
    
    temp <- BASE_data[,c('Beach_FID','LGA_Name')]
    names(temp) <- c('Beach_FID','Beach_LGA_Name')
    LGA_PS <- merge(LGA_PS, temp, by = 'Beach_FID', all.x = TRUE)
    LGA_PS <- merge(LGA_PS, ps_expense, by = 'Beach_LGA_Name', all.x = TRUE)
    
    temp <- df1_s[,c('Beach_FID','international_visit_beach_scenario')] %>%
      distinct()
    LGA_PS <- merge(LGA_PS, temp, by = 'Beach_FID', all.x = TRUE)
    
    temp <- df2_s[,c('Beach_FID','interstate_visit_beach_scenario')] %>%
      distinct()
    LGA_PS <- merge(LGA_PS, temp, by = 'Beach_FID', all.x = TRUE)
    
    temp <- df3_s_1 %>%
      filter(within2hr == 1) %>%
      group_by(Beach_FID) %>%
      summarise(domestic_x_prob_scenario = sum(domestic_x_prob_scenario)) %>%
      rename(within2hr_visitors_scenario = domestic_x_prob_scenario)
    LGA_PS <- merge(LGA_PS, temp, by = 'Beach_FID', all.x = TRUE)
    LGA_PS$within2hr_visitors_scenario[is.na(LGA_PS$within2hr_visitors_scenario)] <- 0
    
    temp <- df3_s_1 %>%
      filter(outside2hr == 1) %>%
      group_by(Beach_FID) %>%
      summarise(domestic_x_prob_scenario = sum(domestic_x_prob_scenario)) %>%
      rename(outside2hr_visitors_scenario = domestic_x_prob_scenario)
    LGA_PS <- merge(LGA_PS, temp, by = 'Beach_FID', all.x = TRUE)
    LGA_PS$outside2hr_visitors_scenario[is.na(LGA_PS$outside2hr_visitors_scenario)] <- 0
    
    rm(temp)
    gc()
    
    LGA_PS$International_PS_Base <- LGA_PS$international_visit_beach_base * LGA_PS$international_expense * 1.11
    LGA_PS$Interstate_PS_Base <- LGA_PS$interstate_visit_beach_base * LGA_PS$overnight_expense * 1.11
    LGA_PS$within2hr_PS_Base <- LGA_PS$within2hr_visitors_base * LGA_PS$day_expense * 1.11
    LGA_PS$outside2hr_PS_Base <- LGA_PS$outside2hr_visitors_base * LGA_PS$overnight_expense * 1.
    
    LGA_PS$Total_PS_Base <- LGA_PS$International_PS_Base + LGA_PS$Interstate_PS_Base + LGA_PS$within2hr_PS_Base + LGA_PS$outside2hr_PS_Base
    
    LGA_PS$International_PS_Scenario <- LGA_PS$international_visit_beach_scenario * LGA_PS$international_expense * 1.11
    LGA_PS$Interstate_PS_Scenario <- LGA_PS$interstate_visit_beach_scenario * LGA_PS$overnight_expense * 1.11
    LGA_PS$within2hr_PS_Scenario <- LGA_PS$within2hr_visitors_scenario * LGA_PS$day_expense * 1.11
    LGA_PS$outside2hr_PS_Scenario <- LGA_PS$outside2hr_visitors_scenario * LGA_PS$overnight_expense * 1.11
    
    LGA_PS$Total_PS_Scenario <- LGA_PS$International_PS_Scenario + LGA_PS$Interstate_PS_Scenario + LGA_PS$within2hr_PS_Scenario + LGA_PS$outside2hr_PS_Scenario
    
    LGA_PS$Total_PS_Change <- LGA_PS$Total_PS_Scenario - LGA_PS$Total_PS_Base
    
    State <- LGA_PS[,c('Beach_FID','Beach_Name')]
    
    State$State_LGA_CS_Base <- unlist(LGA_CS$Total_CS_Base)
    State$State_intraState_CS_Base <- LGA_PS$within2hr_PS_Base + LGA_PS$outside2hr_PS_Base
    
    State$Total_CS_Base <- State$State_LGA_CS_Base + State$State_intraState_CS_Base
    
    State$Total_LGA_PS_Base <- LGA_PS$International_PS_Base + LGA_PS$Interstate_PS_Base
    
    State$State_LGA_CS_Scenario <- unlist(LGA_CS$Total_CS_Scenario)
    State$State_intraState_CS_Scenario <- LGA_PS$within2hr_PS_Scenario + LGA_PS$outside2hr_PS_Scenario
    
    State$Total_CS_Scenario <- State$State_LGA_CS_Scenario + State$State_intraState_CS_Scenario
    
    State$Total_LGA_PS_Scenario <- LGA_PS$International_PS_Scenario + LGA_PS$Interstate_PS_Scenario
    
    # visitor frame
    
    out_visitors <- df3_s_1 %>%
      filter(LGA_Name == input$lga, Beach_FID %in% unique(BASE_data[BASE_data$LGA_Name==input$lga,]$Beach_FID)) %>%
      select(Beach_FID, Beach_Name, domestic_x_prob_base, domestic_x_prob_scenario)
    
    out_visitors <- merge(out_visitors, df1_s[df1_s$LGA_Name==input$lga, c('Beach_FID','international_x_prob_base','international_x_prob_scenario')], by = 'Beach_FID', all.x = TRUE)
    out_visitors <- merge(out_visitors, df2_s[df2_s$LGA_Name==input$lga, c('Beach_FID','interstate_x_prob_base','interstate_x_prob_scenario')], by = 'Beach_FID', all.x = TRUE)
    
    out_visitors <- merge(out_visitors, LGA_CS, by = 'Beach_FID', all.x = TRUE)
    out_visitors <- merge(out_visitors, LGA_PS, by = c('Beach_FID','Beach_Name'), all.x = TRUE)
    
    out_visitors$CS_per_person_Base <- out_visitors$Total_CS_Base/out_visitors$domestic_x_prob_base
    out_visitors$PS_per_person_Base <- out_visitors$Total_PS_Base/out_visitors$domestic_x_prob_base
    
    out_visitors$CS_per_person_Scenario <- out_visitors$Total_CS_Scenario/out_visitors$domestic_x_prob_scenario
    out_visitors$PS_per_person_Scenario <- out_visitors$Total_PS_Scenario/out_visitors$domestic_x_prob_scenario
    
    # #EBO values
    ebo <- read.csv(file = "EBO_input.csv", fileEncoding = "ISO-8859-1", stringsAsFactors = FALSE)
    names(ebo) <- c('Beach_LGA_Name', '2023_household_LGA', '2023_household_NSW')

    ebo <- ebo[,c('Beach_LGA_Name', '2023_household_LGA', '2023_household_NSW')]

    ebo$Beach_LGA_Name[ebo$Beach_LGA_Name == 'Sutherland'] <- 'Sutherland Shire'

    temp <- df3[c('Beach_FID','Beach_LGA_Name','Length_B')]
    temp <- temp[!duplicated(temp), ]
    temp <- aggregate(`Length_B` ~ Beach_LGA_Name, temp, sum)
    names(temp) <- c('Beach_LGA_Name','Total_LGA_length_Base')

    ebo <- merge(ebo, temp, by='Beach_LGA_Name', all.x = TRUE)
    
    print(paste("++++++++++++> Beach_LGA_Name in df3_s_1: ", "Beach_LGA_Name" %in% names(df3_s_1)))
    
    temp <- df3_s_1[c('Beach_FID','Beach_LGA_Name', 'Length_S')]
    temp <- temp[!duplicated(temp), ]
    temp <- aggregate(`Length_S` ~ Beach_LGA_Name, temp, sum)
    names(temp) <- c('Beach_LGA_Name','Total_LGA_length_Scenario')

    ebo <- merge(ebo, temp, by='Beach_LGA_Name', all.x = TRUE)

    ebo$Total_NSW_Length_B <- sum(ebo$Total_LGA_length_Base, na.rm = TRUE)
    ebo$Total_NSW_Length_S <- sum(ebo$Total_LGA_length_Scenario, na.rm = TRUE)

    temp <- df3[c('Beach_FID','Beach_LGA_Name','Length_B')]
    temp <- temp[!duplicated(temp), ]

    ebo <- merge(ebo, temp, by='Beach_LGA_Name', all.x = TRUE)

    temp <- df3_s_1[c('Beach_FID','Beach_LGA_Name','Length_S')]
    temp <- temp[!duplicated(temp), ]

    ebo <- merge(ebo, temp, by=c('Beach_FID','Beach_LGA_Name'), all.x = TRUE)

    ebo$LGA_ebo_base <- ((89.19 * ebo$`2023_household_LGA`)/ebo$Total_LGA_length_Base) * ebo$Length_B
    ebo$LGA_ebo_scenario <- ((89.19 * ebo$`2023_household_LGA`)/ebo$Total_LGA_length_Scenario) * ebo$Length_S
    ebo$LGA_ebo_change <- ebo$LGA_ebo_scenario - ebo$LGA_ebo_base

    ebo$State_ebo_base <- ((89.19 * ebo$`2023_household_NSW`)/ebo$Total_NSW_Length_B) * ebo$Length_B
    ebo$State_ebo_scenario <- ((89.19 * ebo$`2023_household_NSW`)/ebo$Total_NSW_Length_S) * ebo$Length_S
    ebo$State_ebo_change <- ebo$State_ebo_scenario - ebo$State_ebo_base
     
    # Reactive values (assuming 'reactive_values' is a list in Shiny)
    for (i in seq_len(nrow(data_subset_modified()))) {
      
      # updateTextInput(session, paste0("csb", i), paste("$", value = prettyNum(1, big.mark=",", scientific=FALSE)))
      updateTextInput(session, paste0("csb", i), value = paste0("$ ", format(LGA_CS$Total_CS_Base[i], big.mark = ",", scientific = FALSE)))
      updateTextInput(session, paste0("css", i), value = paste0("$ ", format(LGA_CS$Total_CS_Scenario[i], big.mark = ",", scientific = FALSE)))
      updateTextInput(session, paste0("csc", i), value = paste0("$ ", format(LGA_CS$CS_Delta[i], big.mark = ",", scientific = FALSE)))
      
      updateTextInput(session, paste0("psb", i), value = paste0("$ ", format(LGA_PS$Total_PS_Base[i], big.mark = ",", scientific = FALSE)))
      updateTextInput(session, paste0("pss", i), value = paste0("$ ", format(LGA_PS$Total_PS_Scenario[i], big.mark = ",", scientific = FALSE)))
      updateTextInput(session, paste0("psc", i), value = paste0("$ ", format(LGA_PS$Total_PS_Change[i], big.mark = ",", scientific = FALSE)))
      
    }
    
  })
  
}


# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)



























