##################################################################
#### Section 4 Code
##################################################################

#################################
### Comments about this code
#################################

# Data for this analysis code should be set up according to the accompanying data specification guide.

# Code below for this analysis should be modified according to your needs. In some places, comments explicitly prompt
# you to appropriately modify a minimum number of commands for your state's data, or else the output may not be
# correct or useful. And of course feel free to modify any other commands, too.

# R Version 2024.04.2 was used to generate the graphs below. If commands to not run, you will need to upgrade your R version.
	
# The code below varies considerably in complexity depending on which graph is being created.
  
#################################
### Set up and setting file paths
#################################

# load libraries
rm(list = ls())
install.packages("tidyverse")  # Collection of data science packages including ggplot2, dplyr, etc.
install.packages("haven")      # For reading SPSS, Stata, and SAS files
install.packages("rlang")      # For advanced R programming and language manipulation
install.packages("extraDistr")
library(tidyverse)
library(haven)
library(rlang)
library(extraDistr)

# set working directory
setwd("workingdirectory")

# set name of folder to save graphs (must be created in advance)
saved_graphs <- "graphs"

# set plot theme
mytheme <- 
  theme_minimal(base_size = 16) +
  theme(legend.title = element_blank(),
        panel.grid.major = element_line(color = "grey80", size = 0.2, linetype = "dotted"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.caption = element_text(size = 12, margin = margin(t = 20)))
theme_set(mytheme)

# define color palette (default here is color-blind)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F03442", "#0072B2", "#D55E00", "#CC79A7")

#######################################
### Load data set up according to spec
#######################################


df0 <- read_dta("sections_1_2_4.dta") # for .dta files
#df0 <- read_csv("sections_1_2_4" )# for CSV files

##########################################
### Label variables as relevant to state
##########################################

df1 <- df0 %>%
  mutate(
    # female
    SEX = if_else(SEX == 0, "Male", "Female"), # set these values labels according to your preferred conventions; these are examples
    
    # race 
    RACE = factor(RACE,  levels = c(1, 2, 3, 4), labels = c("Asian", "Black", "Hispanic", "White")),
    
    # degree
    HIGHEST_DEGREE_IN_YEAR = factor(HIGHEST_DEGREE_IN_YEAR, 
                                    levels = c(5, 4, 3, 2, 1),
                                    labels = c("Graduate",
                                               "Bachelors",
                                               "Associate",
                                               "Cert./Dip.", 
                                               "HS")))

#################################
### Generate additional variables
#################################

df2 <- df1 %>%
  mutate(
    # generate variable for # years out of high school
    years_from_hs = YEAR - HS_GRADUATION_YEAR) # year is the year in which you're measuring outcomes; HS_GRADUATION_YEAR is the year a student graduated from HS 

#################################
### Example linear imputation for missing wages
#################################

# In the technical guide, we recommended linearly imputing missing wages for those who are missing 1
# wage quarter. Below we provide example code. "wages_q1" through "wages_q4" are variables that contain wage 
# amounts for each quarter in the year you plan to measure wages.  
# 
#   
# ## 1. Begin by regressing the missing quarter on the non-missing quarters for those who have all four quarters present. You will then generate a variable with the predicted wage values from your regression. You will repeat this for each quarter, switching out the dependent variable each time. You will use these predicted values to fill in the missing wage quarter.
# mod_q1 <- lm(wages_q1 ~ wages_q2 + wages_q3 + wages_q4, data = df2)
# mod_q2 <- lm(wages_q2 ~ wages_q1 + wages_q3 + wages_q4, data = df2)
# mod_q3 <- lm(wages_q3 ~ wages_q1 + wages_q2 + wages_q4, data = df2)
# mod_q4 <- lm(wages_q4 ~ wages_q1 + wages_q2 + wages_q2, data = df2)
# 
#df3 <- df2 %>%
# ## 2. Next you will replace the missing wages with imputed values
#   mutate(
#     wages_q1 = if_else(is.na(wages_q1) & !is.na(wages_q2) & !is.na(wages_q3) & !is.na(wages_q4), predict(mod_q1, newdata = .), wages_q1),
#     wages_q2 = if_else(is.na(wages_q2) & !is.na(wages_q1) & !is.na(wages_q3) & !is.na(wages_q4), predict(mod_q2, newdata = .), wages_q2),
#     wages_q3 = if_else(is.na(wages_q3) & !is.na(wages_q1) & !is.na(wages_q2) & !is.na(wages_q4), predict(mod_q3, newdata = .), wages_q3),
#     wages_q4 = if_else(is.na(wages_q4) & !is.na(wages_q1) & !is.na(wages_q2) & !is.na(wages_q3), predict(mod_q4, newdata = .), wages_q4),
#     
# ## 3. Finally, you will sum the quarterly wages together to arrive at the annual wages
#     annualwages = wages_q1 + wages_q2 + wages_q3 + wages_q4)

# If no imputation is performed, df3=df2
df3 <- df2 
  
#############################################
### List variables and parameters of interest
#############################################

years_out_max <- 10 # maximum number of years from HS analyses will consider

vars <- c("SEX", "RACE") # add your demographic variables to the vector here
tests <- c("MATH_TEST_QUARTILE")  # add your test score variables to the vector here

living_wage <- 30000
#################################
### Diagnostic charts
#################################

## Diagnostic Chart 4.a.1: Mean Wages by Highest Degree 10 Years After HS Graduation Benchmarked Against the Living Wage Threshold

# 1. Data set up
out1 <- df3 %>%
  # keeping nth year after HS and if enrolled in college
  filter(years_from_hs == years_out_max) %>%
  drop_na(ADJ_ANNUAL_WAGES) %>%
  
  # collapsing to get percent of individuals in each sector category - these can change based on your data
  group_by(HIGHEST_DEGREE_IN_YEAR) %>%
  dplyr::summarise(mean_wage = mean(ADJ_ANNUAL_WAGES)) %>%
  ungroup() 

# 2. Graph
p1 <- 
  ggplot(out1, aes(x = HIGHEST_DEGREE_IN_YEAR, y = mean_wage)) +
  geom_hline(yintercept = living_wage, linetype = "dotted", color = "red") +
  geom_bar(position = "stack", stat = "identity", width = 0.6) + 
  scale_fill_manual(values = cbPalette) +
  ylab("Adjusted Annual Wages ($)") + 
  xlab("Years Since HS Graduation") +
  ggtitle("Mean Wages by Highest Degree 10 Years After HS Graduation \n Benchmarked Against the Living Wage Threshold") +
labs(caption = "Wages adjusted to 2022 \nWages for HS Classes of x \nDotted line is the average living wage between 2020 and 2022 adjusted to 2022 dollars \nLiving wage estimates from MIT Living Wage Calculator \nSample comprises all individuals with 3 or 4 wage quarters observed in 10th year after HS graduation, N = \nMissing quarter imputed for individuals with only 3 wage quarters observed")


ggsave(p1, file = paste0(saved_graphs, "/mean_raw.png"))


## Diagnostic Chart 4.a.2: Distribution of Wages by Highest Degree 10 Years After HS Graduation Benchmarked Against the Living Wage Threshold

p2 <- 
  df3 %>% filter(years_from_hs == years_out_max) %>%
  ggplot(., aes(x = ADJ_ANNUAL_WAGES)) +
  geom_vline(xintercept = living_wage, linetype = "dotted", color = "red") +
  facet_wrap( ~ HIGHEST_DEGREE_IN_YEAR, nrow = 1) +
  geom_histogram(aes(y = (..count..)/tapply(..count.., ..PANEL.., sum)[..PANEL..] * 100), 
                 fill = "white", color = "black", bins = 10) + 
  ylab("Percent") + 
  xlab("Adjusted Annual Wages ($)") +
  ggtitle("Distribution of Wages by Highest Degree 10 Years After HS Graduation \n Benchmarked Against the Living Wage Threshold") +
  labs(caption = "Wages adjusted to 2022 \nWages for HS Classes of x \nDotted line is the average living wage between 2020 and 2022 adjusted to 2022 dollars \nLiving wage estimates from MIT Living Wage Calculator \nSample comprises all individuals with 3 or 4 wage quarters observed in 10th year after HS graduation, N = \nMissing quarter imputed for individuals with only 3 wage quarters observed")


ggsave(p2, file = paste0(saved_graphs, "/distribution.png"))




## Diagnostic Chart 4.a.3a: Mean Wages by Highest Degree and by Demographic Characteristics 10 Years After HS Graduation Benchmarked Against the Living Wage Threshold

for (var in c(vars)) {
  
  out3a_temp <- df3 %>%
    # Data set up
    filter(years_from_hs == years_out_max) %>%
    drop_na(!!sym(var), ADJ_ANNUAL_WAGES) %>%
    
    # collapse to mean wages by highest degree and demographic characteristic (can change to median by editing function to be `median`)
    group_by(!!sym(var), HIGHEST_DEGREE_IN_YEAR) %>%
    dplyr::summarise(mean_wage = mean(ADJ_ANNUAL_WAGES)) %>%
    dplyr::select(var = !!sym(var),
                  degree = HIGHEST_DEGREE_IN_YEAR,
                  mean_wage)
  
  p3a_temp <- 
    ggplot(out3a_temp, aes(x = var, y = mean_wage, fill = degree)) + 
    geom_hline(yintercept = living_wage, linetype = "dotted", color = "red") + 
    geom_bar(stat = "identity", width = 0.6, position = position_dodge(width = 0.6)) + 
    geom_text(aes(label = round(mean_wage, 0)), 
              position = position_dodge(width = 0.6), 
              vjust = -0.3, hjust = -0.15, size = 3, angle = 90) +  # Angle set to 90 for vertical text
    scale_fill_manual(values = cbPalette) + 
    ggtitle(paste0("Mean Wages by Highest Degree and by ", var, "\n10 Years After HS Graduation Benchmarked Against the Living Wage Threshold")) + 
    ylab("Adjusted Annual Wages ($)") + 
    xlab("") + 
    theme(
      legend.title = element_blank(),
      panel.grid.major.x = element_line(color = "grey80", size = 0.2, linetype = "dotted")
    ) + 
    theme(plot.title=element_text(size=12)) +
    labs(
      caption = "Wages adjusted to 2022\nWages for HS Classes of x\nDotted line is the average living wage between 2020 and 2022 adjusted to 2022 dollars\nLiving wage estimates from MIT Living Wage Calculator\nSample comprises all individuals with 3 or 4 wage quarters observed in 10th year after HS graduation, N = 96,513\nMissing quarter imputed for individuals with only 3 wage quarters observed"
    )
  
  eval(parse_expr(paste0("p3a_", var, "<- p3a_temp")))
  name <- paste0("/mean_", var, "_degree.png")
  ggsave(p3a_temp, file = paste0(saved_graphs, name))
  
}

## Diagnostic Chart 4.a.3b: Mean Wages by Highest Degree and by Demographic Characteristics by Math Score 10 Years After HS Graduation Benchmarked Against the Living Wage Threshold

for (var in c(vars)) {
  
  out3b_temp <- df3 %>%
    filter(MATH_TEST_QUARTILE %in% c(1,4)) %>%
    mutate(math_quart = if_else(MATH_TEST_QUARTILE == 4, 1, 0),
           math_quart = factor(math_quart, levels = 0:1, labels = c("1", "4"))) %>%
    # Data set up
    filter(years_from_hs == years_out_max) %>%
    drop_na(!!sym(var), ADJ_ANNUAL_WAGES) %>%
    
    # collapse to mean wages by highest degree and demographic characteristic (can change to median by editing function to be `median`)
    group_by(!!sym(var), HIGHEST_DEGREE_IN_YEAR, math_quart) %>%
    dplyr::summarise(mean_wage = mean(ADJ_ANNUAL_WAGES)) %>%
    dplyr::select(var = !!sym(var),
                  degree = HIGHEST_DEGREE_IN_YEAR,
                  math_quart = math_quart,
                  mean_wage)
  
  p3b_temp <- ggplot(out3b_temp, aes(x = var, y = mean_wage, fill = degree)) + 
    geom_hline(yintercept = living_wage, linetype = "dotted", color = "red") + 
    geom_bar(stat = "identity", width = 0.6, position = position_dodge(width = 0.6)) + 
    facet_wrap(~ math_quart, nrow = 1) +  
    ggtitle(paste0("Mean Wages by Highest Degree and by ", var, "\n10 Years After HS Graduation Benchmarked Against the Living Wage Threshold")) + 
    ylab("Adjusted Annual Wages ($)") + 
    xlab("") +
    theme(
      legend.title = element_blank(),
      panel.grid.major.x = element_line(color = "grey80", size = 0.2, linetype = "dotted")
    ) + 
    theme(plot.title=element_text(size=12)) +
    labs(
      caption = "Wages adjusted to 2022\nWages for HS Classes of x\nDotted line is the average living wage between 2020 and 2022 adjusted to 2022 dollars\nLiving wage estimates from MIT Living Wage Calculator\nSample comprises all individuals with 3 or 4 wage quarters observed in 10th year after HS graduation, N = 96,513\nMissing quarter imputed for individuals with only 3 wage quarters observed"
    )
  
  
  eval(parse_expr(paste0("p3b_", var, "<- p3b_temp")))
  name <- paste0("/mean_", var, "_degree_test.png")
  ggsave(p3b_temp, file = paste0(saved_graphs, name))
  
}





