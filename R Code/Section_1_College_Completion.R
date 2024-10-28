##################################################################
#### Section 1 Code
##################################################################

#################################
### Comments about this code
#################################

# Data for this analysis code should be set up according to the accompanying data specification guide.

# Code below for this analysis should be modified according to your needs. In some places, comments explicitly prompt
# you to appropriately modify a minimum number of commands for your state's data, or else the output may not be
# correct or useful. And of course feel free to modify any other commands, too.

# R Version 2024.04.2 was used to generate the graphs below. If commands to not run, you will need to upgrade your R version.

# The code below varies considerably in complexity depending on which graph is being created. For example, 
# R does not have a basic command that automatically generates waterfall plots. As such, the code for graph 2 is more complex.


#################################
### Set up and setting file paths
#################################

# load libraries
rm(list = ls())
library(tidyverse)
library(haven)
library(rlang)
library(ggsankey)
library(ggplot2)
library(tidyverse)
install.packages("devtools")
devtools::install_github("davidsjoberg/ggsankey")

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
#df0 <- read_csv("sections_1_2_4.csv" )# for CSV files

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


#############################################
### List variables and parameters of interest
#############################################

years_out_max <- 10 # maximum number of years from HS analyses will consider

vars <- c("SEX", "RACE") # add your demographic variables to the vector here
tests <- c("MATH_TEST_QUARTILE")  # add your test score variables to the vector here

#################################
### Diagnostic charts
#################################

## Diagnostic Chart 1.a.1: Percentage of Graduating Classes with Postsecondary Credentials

# 1. Data set up
out1 <- df2 %>%
  # defining range of x axis
  filter(years_from_hs %in% seq(1, years_out_max)) %>%
  
  # collapsing to get sum of students in each credential category per year
  group_by(years_from_hs, HIGHEST_DEGREE_IN_YEAR) %>%
  dplyr::summarise(n_year = n()) %>%
  ungroup() %>%
  group_by(years_from_hs) %>%
  mutate(n = sum(n_year)) %>%
  ungroup() %>%
  # generating the percent of students in each credential category
  mutate(percent = n_year/n*100) %>%
  
   #generative new variable collapsing small cell sizes
  mutate(n = if_else(HIGHEST_DEGREE_IN_YEAR == "Bachelors" & years_from_hs <= 2 | 
                      HIGHEST_DEGREE_IN_YEAR == "Graduate" & years_from_hs <= 2, 0, as.double(n))) %>%
  filter(HIGHEST_DEGREE_IN_YEAR != "HS")

# 2. Graph
p1 <- ggplot(out1, aes(fill = HIGHEST_DEGREE_IN_YEAR, y = percent, x = years_from_hs)) +
  geom_bar(position = "stack", stat = "identity") + 
  scale_fill_manual(values = cbPalette) +
  ggtitle("Percentage of Graduating Classes with Postsecondary Credentials") + 
  theme(plot.title=element_text(size=12)) +
  ylab("Percentage of Classes") + 
  xlab("Years Since HS Graduation") +
  labs(caption = "SAMPLE INFO HERE") +
  scale_y_continuous(limits = c(0,100), breaks = seq(10,100, by = 10)) +
  scale_x_continuous(breaks = seq(1, max(df2$years_from_hs), by = 1))

ggsave(p1, file = paste0(saved_graphs, "/enroll_over_time.png"))


## Diagnostic Chart 1.a.2: Degree Attainment 10 Years from HS by Demographic/Academic Characteristics

for (var in c(vars, tests)) {
  
  # 1. Data set up
  out2 <- df2 %>%
    # keeping 10th year after HS - this can be modified based on the time horizon in which you want to measure attainment
    filter(years_from_hs == years_out_max) %>%
    mutate(var_x = factor(!!sym(var))) %>%
    drop_na(var_x) %>%
    
    # collapsing to get sum of students in each credential category per year per demographic level
    group_by(HIGHEST_DEGREE_IN_YEAR, var_x) %>%
    dplyr::summarise(n_var_x = n()) %>%
    ungroup() %>%
    group_by(var_x) %>%
    
    # generating the percent of students in each credential category  per demographic level
    mutate(total = sum(n_var_x),
           percent = n_var_x/total*100) %>%
    filter(HIGHEST_DEGREE_IN_YEAR != "HS")

  # 2. Graph
 p2_temp <- 
    ggplot(out2, aes(fill = HIGHEST_DEGREE_IN_YEAR, y = percent, x = var_x)) +
    geom_bar(position = "stack", stat = "identity") + 
    scale_fill_manual(values = cbPalette) +
   ggtitle("Percentage Earned Postsecondary Credential within 10 Years \nof HS by", var) + 
   theme(plot.title=element_text(size=12)) +
   ylab("Percentage of Classes") + 
    xlab("") +
    scale_y_continuous(limits = c(0,100), breaks = seq(10,100, by = 10)) +
    labs(caption = "SAMPLE INFO HERE") 

 eval(parse_expr(paste0("p2_", var, "<- p2_temp")))
 name <- paste0("/per_by_", var, "_pooled.png")
 ggsave(p2_temp, file = paste0(saved_graphs, name))
 
  
}


# Diagnostic Chart 1.a.3: Percentage Earned Postsecondary Credential within 10 Years of HS by Student Demographics and Test Score

for (var in vars) {
  
  for (var2 in tests) {
  
    # 1. Data set up  
    out3_temp <- df2 %>%
      # keeping nth year after HS- this can be modified based on the time horizon in which you want to measure attainment
      filter(years_from_hs == years_out_max) %>%
      mutate(hasdegree = if_else(HIGHEST_DEGREE_IN_YEAR != "HS" & !is.na(HIGHEST_DEGREE_IN_YEAR), 1, 0)) %>%
      drop_na(!!sym(var), !!sym(var2)) %>%
      
      # collapse data to get the number of students in each category 
      group_by(!!sym(var), !!sym(var2)) %>%
      
      # generating percent in each degree category
      dplyr::summarise(percent = mean(hasdegree)*100) %>%
      dplyr::select(var = !!sym(var),
                var2 = !!sym(var2),
                percent)
      
    # 2. Graph
    p3_temp <- 
      ggplot(out3_temp, aes(x = var2, y = percent, color = var, shape = var)) +
      geom_line() +
      geom_point(size = 3) +
      scale_color_manual(values = cbPalette) +
      ggtitle(paste0("Percentage Earned Postsecondary Credential within 10 Years \nof HS by ", var, " and ", var2)) + 
      theme(plot.title=element_text(size=12)) +
      ylab("Percent") + 
      xlab("") +
      scale_y_continuous(limits = c(0,100), breaks = seq(10,100, by = 10)) +
      theme(legend.title = element_blank(),
            panel.grid.major.x = element_line(color = "grey80", size = 0.2, linetype = "dotted")) +
      labs(caption = "SAMPLE INFO HERE") 
    
    eval(parse_expr(paste0("p3_", var, "_", var2, "<- p3_temp")))
    name <- paste0("/prob_deg_attainment_", var, "_", var2, ".png")
    ggsave(p3_temp, file = paste0(saved_graphs, name))
      
  }
}

# Diagnostic Chart 1.a.4: Percentage of Graduating Classes who Completed, are Enrolled, Stopped Out, or Never Attempted College 10 Years from HS Graduation

## (0) Number in total population
total <- df2 %>%
  filter(years_from_hs %in% seq(0, years_out_max)) %>%
  filter(DEG_IN_HS != 1) %>%
  distinct(STUDENT_ID) %>%
  count() %>% as.numeric()

## (1) Generating percent of students who went to college
out4a <- df2 %>%
  
  # drop students who received degree in HS
  filter(DEG_IN_HS != 1) %>%
  
  # keeping 10 years of data
  filter(years_from_hs %in% seq(0, years_out_max)) %>%
  
  # generating ever enrolled flag
  group_by(STUDENT_ID) %>%
  mutate(everenrolled = max(IN_COLLEGE_INDICATOR)) %>%
  ungroup() %>%
  
  # collapsing by every enrolled
  group_by(everenrolled) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  mutate(total = sum(n),
         percent_a = round(n/total*100, digits = 0),
         destination_a = if_else(everenrolled == 1, "Entered College", "Never Entered College")) %>%
  transmute(percent_a, destination_a)


## (2) Generating percent of college attempters who stopped out or never stopped out
out4temp <- df2 %>%
  
  # drop students who received degree in HS
  filter(DEG_IN_HS != 1) %>%
  
  # drop anyone who has not been enrolled
  group_by(STUDENT_ID) %>%
  mutate(everenrolled = max(IN_COLLEGE_INDICATOR)) %>%
  ungroup() %>%
  filter(everenrolled == 1) %>%
  dplyr::select(-everenrolled) %>%
  
  ## only looking at stopout prior to degree completion (this will include the year of degree completion)
  group_by(STUDENT_ID) %>%
  mutate(first_year = min(if_else(HIGHEST_DEGREE_IN_YEAR != "HS", as.numeric(years_from_hs), Inf))) %>%
  ungroup() %>%
  filter(years_from_hs <= first_year) %>% # changed from <
  mutate(hasdegree = if_else(HIGHEST_DEGREE_IN_YEAR != "HS" & !is.na(HIGHEST_DEGREE_IN_YEAR), 1, 0),
         hasdegree = if_else(hasdegree == 1, 2, hasdegree),
         hasdegree = if_else(hasdegree == 0, NA_real_, hasdegree)) %>%
  
  ## only keeping observations after first enrollment
  group_by(STUDENT_ID) %>%
  mutate(first_enroll = min(if_else(IN_COLLEGE_INDICATOR == 1, as.numeric(years_from_hs), Inf))) %>%
  ungroup() %>%
  filter(years_from_hs >= first_enroll) %>%
  dplyr::select(STUDENT_ID, years_from_hs, IN_COLLEGE_INDICATOR, hasdegree) %>%
  
  # reshape
  pivot_wider(names_from = years_from_hs, values_from = c(IN_COLLEGE_INDICATOR, hasdegree)) %>%
  dplyr::select(STUDENT_ID, starts_with("IN_COLLEGE_INDICATOR"), starts_with("hasdegree")) %>%
  
  # ordering data
  # dplyr::select(order(colnames(.))) %>%
  mutate(enrollpattern = apply(dplyr::select(., starts_with("IN_COLLEGE_INDICATOR")), 1, paste, collapse = ""),
         gradpattern = apply(dplyr::select(., starts_with("hasdegree")), 1, paste, collapse = "")) %>%
  
  # ID stopouts using patterns in enrollpattern
  mutate(stopout = if_else(str_detect(enrollpattern, "10"), 1, 0))


out4b <- out4temp %>%
  
  # collapsing to generate percent stopouts
  group_by(stopout) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  transmute(percent_b = round(n/total*100, digits = 0),
            source_b = "Entered College",
            destination_b = if_else(stopout == 1, "Stopped Out", "Never Stopped Out"))


## (3) Generating percent who stopped out and their final outcome (completed, still enrolled, not enrolled)
out4c <- out4temp %>%
  
  # ID grads using patterns in gradpattern
  mutate(grad = if_else(str_detect(gradpattern, "2"), 1, 0)) %>%
  
  # ID still enrolled
  mutate(enrolled = if_else(!!sym(paste0("IN_COLLEGE_INDICATOR_", years_out_max)) == 1 & grad == 0, 1, 0)) %>%
  
  # keeping stopouts
  filter(stopout == 1) %>%
  
  # collapsing to generate percent final outcome
  group_by(grad, enrolled) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  transmute(percent_c = round(n/total*100, digits = 0),
            source_c = "Stopped Out",
            destination_c = case_when(enrolled == 1 & grad == 0 ~ "Enrolled",
                                      grad == 1 ~ "Completed",
                                      enrolled == 0 & grad == 0 ~ "Not Enrolled"))


## (4) Generating percent who never stopped out and their final outcome (completed, still enrolled, not enrolled)
out4d <- out4temp %>%
  
  # ID grads using patterns in gradpattern
  mutate(grad = if_else(str_detect(gradpattern, "2"), 1, 0)) %>%
  
  # ID still enrolled
  mutate(enrolled = if_else(!!sym(paste0("IN_COLLEGE_INDICATOR_", years_out_max)) == 1 & grad == 0, 1, 0)) %>%
  
  # keeping non-stopouts
  filter(stopout == 0) %>%
  
  # collapsing to generate percent final outcome
  group_by(grad, enrolled) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  mutate(percent_d = round(n/total*100, digits = 0),
         source_d = "Never Stopped Out",
         destination_d = case_when(enrolled == 1 ~ "Enrolled",
                                   grad == 1 ~ "Completed")) %>%
  filter(!(grad == 0 & enrolled == 0)) %>%
  transmute(percent_d, source_d, destination_d)


out4 <-
  bind_rows(
    bind_rows(
      (out4a %>% transmute(node = "Graduated from HS",
                           x = "Population",
                           next_node = destination_a,
                           next_x = "College",
                           n = percent_a,
      )),
      out4b %>% transmute(node = source_b,
                          x = "College",
                          next_node = destination_b,
                          next_x = "Stopout",
                          n = percent_b,
      ),
      bind_rows(
        out4c %>%
          bind_rows(out4d %>% transmute(percent_c = percent_d, source_c = source_d, destination_c = destination_d)) %>%
          group_by(destination_c) %>%
          dplyr::summarise(percent_c = sum(percent_c)) %>%
          transmute(node = destination_c,
                    x = "Outcome",
                    next_x = NA_character_,
                    next_node = NA_character_,
                    n = percent_c),
        (out4a %>% transmute(node = "Never Entered College",
                             x = "Outcome",
                             next_node = NA_character_,
                             next_x = NA_character_,
                             n = percent_a,
        ))[1,]
      )),
    out4c %>%
      transmute(x = "Stopout",
                node = source_c,
                next_x = "Outcome",
                next_node = destination_c,
                n = percent_c),
    out4d %>%
      transmute(x = "Stopout",
                node = source_d,
                next_x = "Outcome",
                next_node = destination_d,
                n = percent_d)) %>%
  bind_rows(tibble(node = "Never Entered College", x = "College", next_x = "Outcome", next_node = "Never Entered College", n = as.numeric(out4a[1,1])))


df_expanded <- out4 %>%
  filter(!(node == "Never Entered College" & x == "Outcome")) %>%
  mutate(n = round(n)) %>%
  uncount(weights = n) %>%
  group_by(node, x, next_x) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  dplyr::select(node, x, next_x, next_node, n)

df_expanded <- df_expanded %>%
  mutate(x = factor(x, levels = c("Population", "College", "Stopout", "Outcome")),
         next_x = factor(next_x, levels = c("Population", "College", "Stopout", "Outcome")))

# Plotting Sankey diagram
p4 <- ggplot(df_expanded, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = paste0(node, " ", n))) +
  geom_sankey(flow.alpha = 0.5, color = "gray40", show.legend = F) +
  geom_sankey_label(size = 4, color = "white", fill = "gray40", hjust = 0.2) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"))

name <- paste0("/sankey.png")
ggsave(p4, file = paste0(saved_graphs, name))


#################################
### Additional Charts
#################################
# These graphs are designed to provide context to your analyses.
# For example, if you see that Back students in the first quartile of test scores
# are more likey to complete college than White students in the same quartile
# you may want to know how students are distributed across these quartiles
# by race/ethnicity. Graph 1 accomplishes this. Graph 2 shows the percent of students
# in each demographic category to help you understand the size of your populations of inte

# Graph 1: % of students in each demographic category by test score quartile

  for (var in vars) {
    
    for (var2 in tests) {
      
      out5_temp <- df2 %>%
        filter(years_from_hs == 1) %>%
        mutate(hasdegree = if_else(HIGHEST_DEGREE_IN_YEAR != "HS" & !is.na(HIGHEST_DEGREE_IN_YEAR), 1, 0)) %>%
        drop_na(!!sym(var), !!sym(var2)) %>%
        group_by(!!sym(var), !!sym(var2)) %>%
        dplyr::summarise(n = n()) %>%
        ungroup() %>%
        group_by(!!sym(var)) %>%
        mutate(n_total = sum(n)) %>%
        transmute(var = !!sym(var),
                  var2 = factor(!!sym(var2)),
                  percent = n/n_total*100)
      
      p5_temp <- 
        ggplot(out5_temp, aes(x = var, y = percent, fill = var2)) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(values = cbPalette) +
        ggtitle(paste0("Percent of ", var2, " by ", var)) +
        ylab("Percent") + 
        xlab("") +
        scale_y_continuous(limits = c(0,100), breaks = seq(10,100, by = 10)) +
        labs(caption = "SAMPLE INFO HERE") 
      
      eval(parse_expr(paste0("p5_", var, "_", var2, "<- p5_temp")))
      name <- paste0("/demo_testquartile_", var, "_", var2, ".png")
      ggsave(p5_temp, file = paste0(saved_graphs, name))
      
    }
  }

# Graph 2: % of students in each demographic category 

for (var in vars) {
  
  # 1. Data set up
  out6 <- df2 %>%
    
    # keep years out
    filter(years_from_hs == 1) %>%
    mutate(var_x = factor(!!sym(var))) %>%
    drop_na(var_x) %>%
    
    # collapsing by levels of demographic variable
    group_by(var_x) %>%
    dplyr::summarise(n_var_x = n()) %>%
    ungroup() %>%
    mutate(n = sum(n_var_x)) %>%
    mutate(percent = n_var_x/n*100,
           ypos = cumsum(percent) - 0.5*percent) %>%
    arrange(desc(var_x))
  
  # 2. Graph
  p6_temp <- 
    ggplot(out6, aes(x = "", y = percent, fill = var_x)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = cbPalette) +
    theme_void() +
    theme(legend.title = element_blank(),
          legend.text = element_text(size = 13)) +
    geom_text(aes(y = ypos, label = paste0(round(percent, digits = 2), "%")), color = "white", size = 4) +
    ggtitle(paste0("Percentage of Graduates by ", var)) +
    theme(plot.title = element_text(hjust = 0.5, size = 16)) +
    labs(caption = "SAMPLE INFO HERE") 
  
    eval(parse_expr(paste0("p6_", var, "<- p6_temp")))
    name <- paste0("/desc_", var, "_pooled.png")
    ggsave(p6_temp, file = paste0(saved_graphs, name))
  
}




