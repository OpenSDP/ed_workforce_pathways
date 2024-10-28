##################################################################
#### Section 2 Code
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
#install libraries
install.packages(c('tidyverse', 'haven', 'rlang', 'extraDistr'))

# load libraries
rm(list = ls())
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
                                               "HS"))
  )
    

#################################
### Generate additional variables
#################################

df2 <- df1 %>%
  mutate(
         # generate variable for # years out of high school
         years_from_hs = YEAR - HS_GRADUATION_YEAR) # year is the year in which you're measuring outcomes; HS_GRADUATION_YEAR is the year a student graduated from HS 


#############################################
### List variables and parameters of interest
#############################################

years_out_max <- 5 # maximum number of years from HS analyses will consider

vars <- c("SEX", "RACE") # add your demographic variables to the vector here
tests <- c("MATH_TEST_QUARTILE")  # add your test score variables to the vector here

#################################
### Diagnostic charts
#################################

## Diagnostic Chart 2.a.1: Percentage of Graduating Class Entering College 1-5 Years from High School

# 1. Data set up
out1a <- df2  %>%
  
  # defining range of x axis
  filter(years_from_hs %in% seq(1, years_out_max)) 

for (i in 1:years_out_max) {
  
  out1a <- out1a %>%
    group_by(STUDENT_ID) %>%
    mutate(!!sym(paste0("everenrolled_", i)) := max(IN_COLLEGE_INDICATOR[years_from_hs <= i], na.rm = T)) 
  
}

  out1b <- out1a %>%
    ungroup() %>%
    
  # collapsing to get the sum of ever enrolled by years out from HS
  summarise(across(starts_with("everenrolled"), mean, .names =  "mean_{col}")) %>%
    t() %>% as.numeric()*100
  
  out1c <- 
    
    # creating percentage differences of students entering college per year out 
    cbind(c("One Year", "Two Years", "Three Years", "Four Years", "Five Years", "Sum Enrollment"),
          as.numeric(c(out1b[1], diff(out1b), out1b[years_out_max]))) %>%
    as.data.frame() %>%
    transmute(years_out = V1, percentage = as.numeric(V2)) %>%
    mutate(years_out = factor(years_out, levels = c("One Year", "Two Years", "Three Years", "Four Years", "Five Years", "Sum Enrollment")),
          
           # creating separate numeric x variable for plotting
           x_pos = case_when(years_out == "One Year" ~ 1,
                             years_out == "Two Years" ~ 2,
                             years_out == "Three Years" ~ 3,
                             years_out == "Four Years" ~ 4,
                             years_out == "Five Years" ~ 5,
                             years_out == "Sum Enrollment" ~ 6,
                             TRUE ~ NA_real_),
           
           # creating coordinates to create waterfall-like effect
            bottom = case_when(years_out == "One Year"  ~ 0,
                              years_out %in% c("Two Years", "Three Years", "Four Years") ~ lag(cumsum(percentage), default = 0),
                              years_out == "Five Years" ~ lag(cumsum(percentage), default = 0),
                              years_out == "Sum Enrollment" ~ 0,
                              TRUE ~ NA_real_),
           top = bottom + percentage,
           fill_color = case_when(years_out %in% c("One Year",  "Sum Enrollment") ~ "a",
                                 TRUE ~ "b")) 
    

# 2. Graph
  
 p1 <- ggplot(out1c, aes(ymin = bottom, ymax = top, fill = fill_color)) + 
    geom_rect(aes(xmin = x_pos - .4, xmax = x_pos + .4)) + 
    scale_fill_manual(values = c("b" = "black", "a" = "grey")) +
    ggtitle("Percentage of Graduating Class Entering College 1-5 Years from High School") +
    theme(plot.title=element_text(size=12)) +
    scale_x_continuous(breaks = 1:(years_out_max+1), labels = c("One Year", "Two Years", "Three Years", "Four Years", "Five Years", "Sum Enrollment")) + 
    scale_y_continuous(limits = c(0, 100)) +
    ylab("Percentage of Graduating Classes") +
    xlab("Years from HS") +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(caption = "SAMPLE INCLUSION NOTES")
  
  
 ggsave(p1, file = paste0(saved_graphs, "/pct_enrolled_waterfall.png"))


## Diagnostic Chart 2.a.2: Percentage of Enrolled Graduating Class by College System 1 Year from HS

for (var in vars) {
  
  # 1. Data set up
  out2 <- df2 %>%
    # keeping 1st year after HS and if enrolled in college
    filter(years_from_hs == 1 & IN_COLLEGE_INDICATOR == 1) %>%
    drop_na(SECTOR) %>%
    
    # collapsing to get percent of individuals in each sector category - these can change based on your data
    group_by(SECTOR) %>%
    dplyr::summarise(n_var_x = n()) %>%
    ungroup() %>%
    
    # generating the percent of students in each category
    mutate(total = sum(n_var_x),
           percent = n_var_x/total*100)

  # 2. Graph
 p2 <- 
    ggplot(out2, aes(x = "", y = percent, fill = SECTOR)) +
    geom_bar(width = 1, stat = "identity") + 
    coord_polar("y", start = 0) +
    ylab("") + 
    xlab("") +
   ggtitle("Percentage of Enrolled Graduating Class \n by College System 1 Year from HS") +
    labs(caption = "SAMPLE INFO HERE") 

 ggsave(p2, file = paste0(saved_graphs, "/enroll_system.png"))
 
  
}


## Diagnostic Chart 2.b.1: Percentage Enrolled in College 1 Year after HS by Demographic/Academic Characteristics

for (var in c(vars, tests)) {
    
    out3_temp <- df2 %>%
      filter(years_from_hs == 1) %>%
      drop_na(!!sym(var)) %>%
      group_by(!!sym(var)) %>%
      dplyr::summarise(percent = mean(IN_COLLEGE_INDICATOR)*100) %>%
      dplyr::select(var = !!sym(var),
                    percent)
    
    p3_temp <- 
      ggplot(out3_temp, aes(x = var, y = percent)) +
      geom_bar(stat = "identity") +
      scale_color_manual(values = cbPalette) +
      ggtitle(paste0("Percentage Enrolled  in College 1 Year after HS \nby", var)) +
      ylab("Percentage Enrolled") + 
      xlab("") +
      scale_y_continuous(limits = c(0,100), breaks = seq(10,100, by = 10)) +
      theme(legend.title = element_blank(),
            panel.grid.major.x = element_line(color = "grey80", size = 0.2, linetype = "dotted")) +
      labs(caption = "SAMPLE INFO HERE") 
    
    eval(parse_expr(paste0("p3_", var, "<- p3_temp")))
    name <- paste0("/pct_enrolled_1_yr_out_", var, ".png")
    ggsave(p3_temp, file = paste0(saved_graphs, name))
    
  }
  

## Diagnostic Chart 2.b.2: : Percentage Enrolled in College 1 Year after HS by Demographic Characteristics and Test Score


for (var in vars) {
  
  for (var2 in tests) {
    
    out4_temp <- df2 %>%
      filter(years_from_hs == 1) %>%
      drop_na(!!sym(var), !!sym(var2)) %>%
      group_by(!!sym(var), !!sym(var2)) %>%
      dplyr::summarise(percent = mean(IN_COLLEGE_INDICATOR)*100) %>%
      dplyr::select(var = !!sym(var),
                var2 = !!sym(var2),
                percent)
      
    p4_temp <- 
      ggplot(out4_temp, aes(x = var2, y = percent, color = var, shape = var)) +
      geom_line() +
      geom_point(size = 3) +
      scale_color_manual(values = cbPalette) +
      ggtitle(paste0("Percentage Enrolled in College One Year after High School \nby ", var, " and ", var2)) +
      ylab("Percentage Enrolled") + 
      xlab("") +
      scale_y_continuous(limits = c(0,100), breaks = seq(10,100, by = 10)) +
      theme(legend.title = element_blank(),
            panel.grid.major.x = element_line(color = "grey80", size = 0.2, linetype = "dotted")) +
      labs(caption = "SAMPLE INFO HERE") 
    
    eval(parse_expr(paste0("p4_", var, "_", var2, "<- p4_temp")))
    name <- paste0("/prob_enroll_", var, "_", var2, ".png")
    ggsave(p4_temp, file = paste0(saved_graphs, name))
     
     

  }
}

