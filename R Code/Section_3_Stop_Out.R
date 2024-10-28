##################################################################
#### Section 3 Code
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
library(tidyverse)
library(haven)
library(rlang)
library(extraDistr)
library(dplyr)

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

df0 <- read_dta("section3.dta") # for .dta files
#df0 <- read_csv("section3.csv" ) # for CSV files
    
##########################################
### Label variables as relevant to state
##########################################


df1 <- df0 %>%
  mutate(
    # female
    SEX = if_else(SEX == 0, "Male", "Female"), # set these values labels according to your preferred conventions; these are examples
    
    # race 
    RACE = factor(RACE,  levels = c(1, 2, 3, 4), labels = c("Asian", "Black", "Hispanic", "White"))
    )


#############################################
### List variables and parameters of interest
#############################################

# generate credit minimums by degree, for example, the minimum N credits needed to earn a BA may be 115 
deg_minimum = 115 # this will vary based on your context

# generating last term observed in your data in order to keep only those who had at least 5 years to re-enroll after stop out
last_term_observed = 10

vars <- c("SEX", "RACE") # add your demographic variables to the vector here
tests <- c("MATH_TEST_QUARTILE")  # add your test score variables to the vector here

#################################
### Diagnostic charts
#################################

## Diagnostic Chart 3.a.1: Percentage of Postsecondary Attempters by Enrollment Status 6 Years of College Entry by Demographic/Academic Charactaristics

for (var in vars) {
  
# (1) Data set up
  out1_temp <- df1 %>%
    # dropping those who completed a degree in high school 
    # OUTCOME_MEASURED is an indicator for whether the student should be included in the analysis. See technical narrative for more details
    filter(DEG_IN_HS == 0, OUTCOME_MEASURED == 1) %>%
    
    # collapse data to get the number of students in each category 
    group_by(!!sym(var)) %>%
    
    # generating the percent of students in each category
    dplyr::summarise(across(c(STOPOUT, DEGREE_COMPLETER, CURRENTLY_ENROLLED), ~ mean(.x, na.rm = T)*100)) %>%
    mutate(STOPOUT = -STOPOUT) %>%
    pivot_longer(cols = STOPOUT:CURRENTLY_ENROLLED, names_to = "type", values_to = "percent") %>%
    
    # prepare variables for chart	
    mutate(type = factor(type, levels = c("CURRENTLY_ENROLLED", "DEGREE_COMPLETER", "STOPOUT"), 
                         labels  =c("Enrolled, No Degree",
                                   "Postsecondary Completer",
                                   "Stopped Out, No Degree")))
  
# (2) Graph
  # note: change the title for this graph if you are measuring enrollment status at a time other than 6 years from college entry
  p1_temp <- ggplot(out1_temp, aes(x = SEX, y = percent, fill = type)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_y_continuous(limits = c(-100, 100), labels = abs) +
    scale_fill_manual(values = cbPalette) +
    ggtitle(paste0("Percentage of Postsecondary Attempters by Enrollment Status \n 6 Years of College Entry by ", var)) +
    theme(plot.title=element_text(size=12)) +
    ylab("Percentage of Postsecondary Attempters") + 
    xlab("") +
    labs(caption = "Sample includes the HS classes of X measured at 6 years after college entry (X% of attempters) \nStopout defined as students who have enrolled in at least 1 semester of college \nhave not completed a degree, and was not enrolled in the year measured.") +
    theme(plot.caption=element_text(size=7)) 
  
  eval(parse_expr(paste0("p1_", var, "<- p1_temp")))
  name <- paste0("/stopout_", var, ".png")
  ggsave(p1_temp, file = paste0(saved_graphs, name))
  
}

## Diagnostic Chart 3.a.2: Percentage of Postsecondary Attempters who Stopped Out by Demographic Characteristics and Test Score


for (var in vars) {
  
  for (var2 in tests) {
    
    # 1. Data set up
    out2_temp <- df1 %>%
      # dropping those who completed a degree in high school 
      # keeping those who enrolled with at least 6 years to complete (this can change depending on your context, you may want to measure within 3 years of entry if you are looking at sub-BA attemtpers)
      filter(DEG_IN_HS == 0, OUTCOME_MEASURED == 1) %>%
      drop_na(!!sym(var), !!sym(var2)) %>%
      
      # collapse data to get the number of students in each category 
      group_by(!!sym(var), !!sym(var2)) %>%
      
      # gen percent of students that stopped out
      dplyr::summarise(percent = mean(STOPOUT)*100) %>%
      dplyr::select(var = !!sym(var),
                    var2 = !!sym(var2),
                    percent)
    
    # 2. Graph
    p2_temp <- 
      ggplot(out2_temp, aes(x = var2, y = percent, color = var, shape = var)) +
      geom_line() +
      geom_point(size = 3) +
      scale_color_manual(values = cbPalette) +
      ggtitle(paste0("Percentage Earned Postsecondary Credential \nwithin 10 Years of HS by ", var, " and", var2)) +
      theme(plot.title=element_text(size=12)) +
      ylab("Percent") + 
      xlab("") +
      scale_y_continuous(limits = c(0,100), breaks = seq(10,100, by = 10)) +
      theme(legend.title = element_blank(),
            panel.grid.major.x = element_line(color = "grey80", size = 0.2, linetype = "dotted")) +
      labs(caption = "SAMPLE INFO") 
    
    eval(parse_expr(paste0("p2_", var, "_", var2, "<- p2_temp")))
    name <- paste0("/prob_stopout_", var, "_", var2, ".png")
    ggsave(p2_temp, file = paste0(saved_graphs, name))
    
  }
}


# Diagnostic Chart 3.a.3: Percentage of Degree Attempters who Stopped Out by Number of Credits Earned in First Semester 

  # 1. Data set up
  out3a <- df1 %>%
    # dropping those who completed a degree in high school and who attempted a certain degree in the first term
    # keeping those who enrolled with at least 6 years to complete (this can change depending on your context, you may want to measure within 3 years of entry if you are looking at sub-BA attemtpers)
    filter(DEG_IN_HS == 0, OUTCOME_MEASURED == 1, ATTEMPTED_DEGREE_FIRST_TERM == "Bachelors") %>%
    mutate(years_between = STOP_OUT_TERM-FIRST_TERM,
           
           # identifying those who stopped out within 6 years of postsecondary entry by subtracting the stop out term from a students' first term
           # students who stopped out within 6 years of entry. The number here will also depend on how your data are structured (i.e. whether your data use calendar year or school year). You will change this if you are looking at stop out within another timeframe
           stopout = case_when(years_between <= 6 ~ 1, # students who stopped out within 6 years of entry. The number here will also depend on how your data are structured (i.e. whether your data use calendar year or school year). You will change this if you are looking at stop out within another timeframe
                               is.na(STOP_OUT_TERM) ~ 0, # these are students who never stopped out 
                               TRUE ~ NA_real_)) %>%
    
  # dropping those whose stop out status is unclear (if applicable)
  filter(!is.na(stopout)) %>%
  filter(CREDIT_HOURS_FIRST_TERM >= 0 & CREDIT_HOURS_FIRST_TERM <= 18) %>% #0-99th pctile, this will change based on your data
    
    # binning hours
    mutate(bin = case_when(CREDIT_HOURS_FIRST_TERM==0 ~ 0,
                           CREDIT_HOURS_FIRST_TERM %in% c(1:3) ~ 3,
                           CREDIT_HOURS_FIRST_TERM %in% c(4:6) ~ 6,
                           CREDIT_HOURS_FIRST_TERM %in% c(7:9) ~ 9,
                           CREDIT_HOURS_FIRST_TERM %in% c(10:12) ~ 12,
                           CREDIT_HOURS_FIRST_TERM %in% c(13:15) ~ 15,
                           CREDIT_HOURS_FIRST_TERM %in% c(16:18) ~ 18,
                           )) %>%
    mutate(bin = factor(bin, levels = c("0", "3", "6", "9", "12", "15", "18"))) %>%
    drop_na(bin) %>%
    
    # collapse data by bin
    group_by(bin) %>%
    
    # generate percentage who stopped out in each bin
    dplyr::summarise(percent = mean(stopout)*100)
  
  # 2. Graph
  p3a <- ggplot(out3a, aes(y = percent, x = bin)) +
    geom_bar(position = "stack", stat = "identity") + 
    scale_fill_manual(values = cbPalette) +
    ylab("Percentage") + 
    xlab("Credits Earned in the First Term") +
    ggtitle("Percentage of Degree Attempters who Stopped Out by Number of \n Credits Earned in First Semester") +
    theme(plot.title=element_text(size=12)) +
    labs(caption = "Credits earned in the first term are grouped in to three-credit intervals, \nlabeled by the maximum of the interval \nSample includes classes of x measured within 6 years after college entry. \nStopout defined as students who have enrolled in at least 1 semester of college \nand have not completed a degree. Institution taken from first term of enrollment.") +
    theme(plot.caption=element_text(size=9)) 
  
  ggsave(p3a, file = paste0(saved_graphs, "/stopout_credaccum_BA.png"))
  
  
# Diagnostic Chart 3.a.3: Percentage of Degree Attempters who Stopped Out by GPA Earned in First Semester 

# 1. Data set up
out3b <- df1 %>%
  # dropping those who completed a degree in high school and who attempted a certain degree in the first term
  # keeping those who enrolled with at least 6 years to complete (this can change depending on your context, you may want to measure within 3 years of entry if you are looking at sub-BA attemtpers)
  filter(DEG_IN_HS == 0, OUTCOME_MEASURED == 1, ATTEMPTED_DEGREE_FIRST_TERM == "Bachelors") %>%
  mutate(years_between = STOP_OUT_TERM-FIRST_TERM,
         
         # identifying those who stopped out within 6 years of postsecondary entry by subtracting the stop out term from a students' first term
         # students who stopped out within 6 years of entry. The number here will also depend on how your data are structured (i.e. whether your data use calendar year or school year). You will change this if you are looking at stop out within another timeframe
         stopout = case_when(years_between <= 6 ~ 1, # students who stopped out within 6 years of entry. The number here will also depend on how your data are structured (i.e. whether your data use calendar year or school year). You will change this if you are looking at stop out within another timeframe
                             is.na(STOP_OUT_TERM) ~ 0, # these are students who never stopped out 
                             TRUE ~ NA_real_)) %>%
  
  # dropping those whose stop out status is unclear (if applicable)
  filter(!is.na(stopout)) %>%

  # binning GPA
  mutate(bin = case_when(GPA_FIRST_TERM==0 ~ 1,
                         GPA_FIRST_TERM > 0 &  GPA_FIRST_TERM < 1 ~ 2,
                         GPA_FIRST_TERM >= 1 &  GPA_FIRST_TERM < 2 ~ 3,
                         GPA_FIRST_TERM >= 2  &  GPA_FIRST_TERM < 3 ~ 4,
                         GPA_FIRST_TERM >= 3  &  GPA_FIRST_TERM < 4 ~ 5,
                         GPA_FIRST_TERM == 4 ~ 6,
  )) %>%
  mutate(bin = factor(bin, levels = c("1", "2", "3", "4", "5", "6"),
                      labels = c("0", ">0-.99", "1.0-1.99", "2.0-2.99", "3.0-3.99", "4.0"))) %>%
  drop_na(bin) %>%
  # collapse data by bin
  group_by(bin) %>%
  
  # generate percentage who stopped out in each bin
  dplyr::summarise(percent = mean(stopout)*100)

# 2. Graph
p3b <- ggplot(out3b, aes(y = percent, x = bin)) +
  geom_bar(position = "stack", stat = "identity") + 
  scale_fill_manual(values = cbPalette) +
  ylab("Percentage") + 
  xlab("GPA") +
  ggtitle("Percentage of Degree Attempters who Stopped Out by \n GPA in First Semester") +
  labs(caption = "Sample includes class of X measured at X years after college entry (X% of attempters) \n Stopout defined as students who have enrolled in at least 1 semester of college \nhave not completed a degree, and were not enrolled in the year measured \nCredits include those earned at all institutions in which a student was enrolled")

ggsave(p3b, file = paste0(saved_graphs, "/stopout_gpa_BA.png"))


# Diagnostic Chart 3.a.4: Percentage of Stop Outs by Number Credits Away from Degree Minimums

# 1. Data set up
out4 <- df1 %>%
  # dropping those who completed a degree in high school and who attempted a certain degree in the last semester prior to stop out
  # keeping those who enrolled with at least 6 years to complete (this can change depending on your context, you may want to measure within 3 years of entry if you are looking at sub-BA attemtpers)
  filter(DEG_IN_HS == 0, OUTCOME_MEASURED == 1, ATTEMPTED_DEGREE_STOP_OUT == "Bachelors") %>%
  mutate(years_between = STOP_OUT_TERM-FIRST_TERM,
         
         # identifying those who stopped out within 6 years of postsecondary entry by subtracting the stop out term from a students' first term
         # students who stopped out within 6 years of entry. The number here will also depend on how your data are structured (i.e. whether your data use calendar year or school year). You will change this if you are looking at stop out within another timeframe
         stopout = case_when(years_between <= 6 ~ 1, # students who stopped out within 6 years of entry. The number here will also depend on how your data are structured (i.e. whether your data use calendar year or school year). You will change this if you are looking at stop out within another timeframe
                             is.na(STOP_OUT_TERM) ~ 0, # these are students who never stopped out 
                             TRUE ~ NA_real_)) %>%
  
  # keeping stopouts
  filter(stopout == 1) %>%
  
  mutate(credits_away = deg_minimum - CREDIT_HOURS_CUMMULATIVE) %>%
  
  # binning credits
  mutate(bin = case_when(credits_away==0 ~ 1,
                         credits_away >= 1 &  credits_away <= 14 ~ 2,
                         credits_away >= 15 &  credits_away <= 29 ~ 3,
                         credits_away >= 30  &  credits_away <= 44 ~ 4,
                         credits_away >= 45  &  credits_away <= 59 ~ 5,
                         credits_away >= 60  &  credits_away <= 74 ~ 6,
                         credits_away >= 75  &  credits_away <= 89 ~ 7,
                         credits_away >= 90  &  credits_away <= 104 ~ 8,
                         credits_away >= 105 &  !is.na(credits_away) ~ 9,
                         TRUE ~ NA_real_,
  )) %>%
  mutate(bin = factor(bin, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
                      labels = c("0", "1-14", "15-29", "30-44", "45-59", "60-74",
                                 "75-89", "90-104", ">105"))) %>%
  drop_na(bin) %>%
  
  # collapse data by bin
  group_by(bin) %>%
  
  # generate percentage who stopped out in each bin
    dplyr::summarise(count = n()) %>%
    mutate(percentage = count/sum(count)*100) %>%
    ungroup()
  
# 2. Graph
p4 <- ggplot(out4, aes(y = percentage, x = bin)) +
  geom_bar(position = "stack", stat = "identity") + 
  scale_fill_manual(values = cbPalette) +
  ylab("Percentage of Stop Outs") + 
  xlab("") +
  ggtitle("Percentage of Stop Outs by Number Credits Away from \n ATTEMPTED_DEGREE_STOP_OUT Degree Minimum") +
  labs(caption = "Stopout defined as students who enrolled in at least 1 semester of college \nhad not completed a degree, and was not enrolled at year X \nCredits include those earned at all institutions in which a student was enrolled")

ggsave(p4, file = paste0(saved_graphs, "/stopout_degmins_BA.png"))


# Diagnostic Chart 3.b.1 : Percentage of Stop Outs Who Re-Enrolled Within Five Years of Departure

  # 1. Data set up
  out5a <- df1 %>%
  # dropping those who completed a degree in high school
  # keeping those who enrolled with at least 6 years to complete (this can change depending on your context, you may want to measure within 3 years of entry if you are looking at sub-BA attemtpers)
  filter(DEG_IN_HS == 0, OUTCOME_MEASURED == 1) %>%
  mutate(years_between = STOP_OUT_TERM-FIRST_TERM,
         
         # identifying those who stopped out within 6 years of postsecondary entry by subtracting the stop out term from a students' first term
         # students who stopped out within 6 years of entry. The number here will also depend on how your data are structured (i.e. whether your data use calendar year or school year). You will change this if you are looking at stop out within another timeframe
         stopout = case_when(years_between <= 6 ~ 1, # students who stopped out within 6 years of entry. The number here will also depend on how your data are structured (i.e. whether your data use calendar year or school year). You will change this if you are looking at stop out within another timeframe
                             is.na(STOP_OUT_TERM) ~ 0, # these are students who never stopped out 
                             TRUE ~ NA_real_)) %>%
  
  # keeping stopouts
  filter(stopout == 1) %>%
  
  # keeping those who had at least 5 years to re-enroll after stop out. You will accomplish this by subtracting 5 (or however long you'd like to give students to re enter) from the last term avaialble in your data
  mutate(last_term = last_term_observed - 5) %>%
  
  # keeping if students stopped out prior to 5 years before the end of the panel
  filter(STOP_OUT_TERM <= last_term) %>%

  # generating time between stop out and re-enrollment 
  mutate( time_between= if_else(is.na(REENROLL_SEMESTER), 0, REENROLL_SEMESTER-STOP_OUT_TERM))
  
  # 2. Prep for graph
  tempa <-
    out5a %>%
    
    # generating percent of stopouts who came back after x years
    group_by(time_between) %>%
    dplyr::summarise(count = n()) %>%
    mutate(percentage = count/sum(count)*100) %>%
    ungroup() %>%
    mutate(time_between = if_else(time_between == 0 | time_between > 5, 6, time_between)) %>%
    filter(time_between != 6) %>% dplyr::select(-count)
  
  out5b <-
    
    # adding rows for sum of re-enrollment, sum of non re-enrollment, and total stopouts
    rbind(tempa,
          c(6, sum(tempa$percentage)),
          c(7, 100 - sum(tempa$percentage)),
          c(8, 100)) %>%
    
    # this 
    mutate(time_between = factor(time_between, levels = c("1", "2", "3", "4", "5", "6", "7", "8"),
                                 labels = c("One", "Two", "Three", "Four", "Five", "Sum of Re-Enrolled",
                                            "Sum of Not Re-Enrolled", "Total Stop Outs")),
           
           # creating separate numeric x variable for plotting
           x_pos = case_when(time_between == "One" ~ 1,
                             time_between == "Two" ~ 2,
                             time_between == "Three" ~ 3,
                             time_between == "Four" ~ 4,
                             time_between == "Five" ~ 5,
                             time_between == "Sum of Re-Enrolled" ~ 6,
                             time_between == "Sum of Not Re-Enrolled" ~ 7,
                             time_between == "Total Stop Outs" ~ 8,
                             TRUE ~ NA_real_),
           
           # creating coordinates to create waterfall-like effect
           bottom = case_when(time_between == "One"  ~ 0,
                              time_between %in% c("Two", "Three", "Four") ~ lag(cumsum(percentage), default = 0),
                              time_between == "Five" ~ lag(cumsum(percentage), default = 0),
                              time_between == "Sum of Re-Enrolled" ~ 0,
                              time_between == "Sum of Not Re-Enrolled" ~ lag(cumsum(percentage), n = 2, default = 0),
                              time_between == "Total Stop Outs" ~ 0,
                              TRUE ~ NA_real_),
           top = bottom + percentage,
           fill_color = case_when(time_between %in% c("One", "Two", "Three", "Four", "Five", "Sum of Re-Enrolled") ~ "Re-Enrolled",
                                  time_between == "Sum of Not Re-Enrolled" ~ "Not Re-Enrolled",
                                  time_between == "Total Stop Outs" ~ NA_character_)) 
  
  
  # 3. Graph
 p5 <- ggplot(out5b %>% filter(!is.na(fill_color)), aes(ymin = bottom, ymax = top, fill = fill_color)) + 
    geom_rect(aes(xmin = x_pos - .4, xmax = x_pos + .4)) + 
    scale_fill_manual(values = c("Re-Enrolled" = "grey", "Not Re-Enrolled" = "black"),
                      labels = c("Re-Enrolled" = "Re-Enrolled", "Not Re-Enrolled" = "Not Re-Enrolled")) +
    ggtitle("Percentage of Stop Outs Who Re-Enrolled Within Five Years of Departure") +
    theme(plot.title=element_text(size=12)) +
    scale_y_continuous() +
    scale_x_continuous(breaks = 1:8, labels = c("One", "Two", "Three", "Four", "Five", "Sum of Re-Enrolled",
                                                "Sum of Not Re-Enrolled", "Total Stop Outs")) + 
    
    ylab("Percentage of Stop Outs") +
    xlab("Years Since Stop Out") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    # handling "Total Stop Outs" bar separately for mixed color scheme
    geom_rect(data = out5b %>% filter(time_between == "Total Stop Outs"),
              aes(xmin = x_pos - .4, xmax = x_pos + .4,
                  ymin = 0, ymax = as.numeric(sum(tempa$percentage))), fill = "grey") +
    geom_rect(data = out5b %>% filter(time_between == "Total Stop Outs"),
              aes(xmin = x_pos - .4, xmax = x_pos + .4,
                  ymin = as.numeric(sum(tempa$percentage)), ymax = 100), fill = "black") +
    theme(legend.position = "right") +
    labs(caption = "Sample includes classes of x who stopped out within 6 years of college entry, \nand had 5 years to re-enter. Stop outs defined as those who were absent from enrollment, \nfor at least one year prior to earning their first credential") +
   theme(plot.caption=element_text(size=9)) 
  
 ggsave(p5, file = paste0(saved_graphs, "/stopout_reenroll.png"))
 
