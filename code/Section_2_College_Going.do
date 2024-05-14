
/*******************************************************************************
	Section 2 Code
*******************************************************************************/

	/*****************************************
	 Comments about this code
	*****************************************/
	
	// Data for this analysis code should be set up according to the accompanying data specification guide.
	
	// Code below for this analyis should be modified according to your needs. In some places, comments explicitly prompt
	// you to appropriately modify a minimum number of commands for your state's data, or else the output may not be
	// correct or useful. And of course feel free to modify any other commands, too.
	
	// Stata version 17 was used to generate the graphs below. If commands to not run, you will need to upgrade your Stata verion.
	
	// The code below varies considerably in complexity depending on which graph is being created. For example, 
	// Stata does not have a command that automatically generates waterfall plots. As such, the code for graph 2 is more complex.
	
	// If you do not already have the user-written Stata color scheme plotplainblind installed, please see Stata's website for more guidance on 
	// how to install this. The appropriate installation approach might depend on the environment in which you are using Stata.
	

	
	/***************************************
	 Set up and setting filepaths
	***************************************/
	
	version 17.0
	clear all
	macro drop all	
	set scheme plotplainblind //colorblind-friendly graph scheme
	
	global path_to_data		"some_directory/perhaps_another_directory/maybe_even_one_more_directory/your_data.csv" // set path to your saved .csv data file matching spec document
	global saved_graphs		"some_directory/perhaps_another_directory/finally_the_directory_where_you_would_like_your_graphs_saved" // set path for where you'd like graphs saved on your computer
	//Topmatter
	
	
	/***************************************
	 Load data set up according to spec
	***************************************/
	
	import delimited "${path_to_data}"
	
	
	/********************************************
	 Label variables as relevant to state
	********************************************/
	
	* female
		label define male_vals 0 "Male" 1 "Female" // set these value labels according to your preferred conventions, these are examples
		label values SEX male_vals
	
	* race
		label define race_vals 1 "Asian" 2 "Black" 3 "Hispanic" 4 "White" 
		label values RACE race_vals
		
	* race
		label define degree_vals 1 "HS/GED" 2 "Certificate/Diploma" 3 "Associate" 4 "Bachelor's" 5 "Graduate"
		label values HIGHEST_DEGREE_IN_YEAR degree_vals
		
	* outcome (enrolled in college in specified year)
		label define outcome_vals 0 "Not Enrolled" 1 "Enrolled"
		label values IN_COLLEGE_INDICATOR outcome_vals
		label var IN_COLLEGE_INDICATOR "Student Enrollment Status"
	
	/******************************************
	 Generate additional variables
	******************************************/
	
	*Generating indicator for N years out of high school 
		gen years_from_hs=.
		replace years_from_hs=YEAR-HS_GRADUATION_YEAR 
		//year_measured is the year in which you're measuring outcomes, hs_graduation_year is the year a student graduated from HS.		

	/******************************************
	 Diagnostic Charts
	******************************************/	

	///Diagnostic Chart 2.a.1: Percentage of Graduating Class Entering College 1-5 Years from High School
	{
			preserve
			
			*1. Data set up
				
				*keeping 1-5 years after HS- this can be modified based on the time horizon in which you want to measure enrollment
					keep if years_from_hs>=1 & years_from_hs<=5
				
				*Generating counter for collapse 
					gen counter=1
								
					forvalues i=1/5 { //values here can be modified based on what time horizon you want to measure enrollment
						bysort STUDENT_ID : egen everenrolled_`i'=max(IN_COLLEGE_INDICATOR) if years_from_hs<=`i'
						}	//indicator for whether a student was ever enrolled in each year out of college. 
							//If a student begins college in year 2, but stops out in year 3, they will still recieve a 1 in years 4 and 5.

				*collapsing to get the sum of everenrolled by years out from HS
					collapse (sum) counter everenrolled_1-everenrolled_5 , by (years_from_hs)
					egen total=sum(counter)	
					
				*carry forward everenrolled_1-everenrolled_5 to each obervation
					forvalues i=1/5 {
					replace everenrolled_`i'=. if everenrolled_`i'==0
					carryforward everenrolled_`i', replace
					}
					
				*generating the percent of students in each category
					gen pct_enrolled_base1=0 //this is the first bar
					replace pct_enrolled_base1=(everenrolled_1/counter)*100 if years_out==1
					
					gen pct_enrolled_base=0 //these are the bars for years 2-5
					replace pct_enrolled_base=(everenrolled_1/counter)*100 if years_out==2
					replace pct_enrolled_base=(everenrolled_2/counter)*100 if years_out==3
					replace pct_enrolled_base=(everenrolled_3/counter)*100 if years_out==4
					replace pct_enrolled_base=(everenrolled_4/counter)*100 if years_out==5
				
				*this code adds a column which contains the percent of students ever enrolled by the following year
				*this allows us to calculate the difference in percent ever enrolled year to year
					gen pct_enrolled_additional=0
						forvalues i=1/5 {
							replace pct_enrolled_additional=(everenrolled_`i'/counter)*100 if years_out==`i'
						}
				
				*calculate difference in percent ever enrolled year to year
					gen pct_enrolled_yr_over_yr=.
					replace pct_enrolled_yr_over_yr= round(pct_enrolled_additional-pct_enrolled_base)
				
					keep pct_enrolled_yr_over_yr years_out
				
				*adding row for sum of enrollment
					set obs `=_N+1'
					replace years_out=6 in `=_N' //can change the 6 based on your time horizon. If you are measure enrollment in years 1-7 out of HS you could change this to an 8
					sum pct_enrolled_yr_over_yr
					replace pct_enrolled_yr_over_yr=`r(sum)' in `=_N'
					
					gen id=_n
					order id
				
				*generating groups to categorize positive and negative flows year to year, you should not need to make changes here
					cap drop group
					gen group=.
					replace group=1 if years_out==1 | id==`=_N'
					replace group=2 if pct_enrolled_yr_over_yr >=0 & group==.
					replace group=3 if pct_enrolled_yr_over_yr <0 & group==.

					cap drop hi lo
					gen lo=.
					gen hi=.
					replace lo=0 if group==1
					replace hi=pct_enrolled_yr_over_yr if group==1

				forval i=2/`=_N-1' {
					replace lo=hi[_n-1] in `i'
					replace hi=lo+pct_enrolled_yr_over_yr in `i'
				}
				
				*generating x axis labels and spacing them apart
					gen x=.
					replace x=years_out*2
					label define yrlab 2 "One Year" 4 "Two Year" ///
					6 "Three Year" 8 "Four Year" 10 "Five Year" 12 "Sum Enrollment" //this will change based on your time horizon
					label values x yrlab

				*adding leading lines to go between bars
					gen li_start=x
					gen li_end=x[_n+1]
					
				*setting up labels
					cap drop pos
					gen pos=hi>lo

					cap drop ylab 
					gen ylab=.

					replace ylab=hi+5 if pos==1
					replace ylab=hi-5 if pos==0

			*2. Graph
					twoway ///
					(rbar lo hi x if group==2, fi(80) barwidth(1.5)) ///
					(rbar lo hi x if group==3, fi(80) barwidth(1.5)) ///
					(rbar lo hi x if group==1, fi(80) fc(gs12) barwidth(1.5)) ///
					(pcspike hi li_start hi li_end, lc(black) lw(0.08)) ///
					(scatter ylab x if pos==1, mlab(pct_enrolled_yr_over_yr) mlabs(2.5) mlabpos(6)  mc(none)) ///
					(scatter ylab x if pos==0, mlab(pct_enrolled_yr_over_yr) mlabs(2.5) mlabpos(12)  mc(none)) ///
					, legend(off) xlabel(2(2)12, valuelabel) ///
					title("Percentage of Graduating Class Entering College 1-5 Years from High School") ///
					ytitle("Percentage of Graduating Classes") xtitle("Years from HS") ///
					note("SAMPLE INCLUSION NOTES", size(vsmall))

					graph export "${saved_graphs}/pct_enrolled_waterfall.pdf", replace
			restore
	}

	//Diagnostic Chart 2.a.2: Percentage of Enrolled Graduating Class by College System 1 Year from HS
	{
			preserve
			*1. Data set up
				*keeping 1st year after HS
					keep if years_out==1
					
				*keeping if enrolled in college
					keep if IN_COLLEGE_INDICATOR==1
				
				*generating counter to get total in each category
					gen counter=1 
					
				*collapsing to get sum in each sector
					collapse (sum) counter, by(SECTOR)
					egen total=sum(counter)		
					
				*generating percent in each category, these can change based on your data
					gen public_instate=counter/total if SECTOR=="public_instate"
					gen private_instate=counter/total if SECTOR=="private_instate"
					gen out_of_state=counter/total if SECTOR=="out_of_state"
				
			*2. Graph
				graph pie public_instate private_instate out_of_state ,  ///
				title("Percentage of Enrolled Graduating Class" "by College System 1 Year from HS") ///
				note("Sample only includes students enrolled in 1 college", size(small)) ///
				legend (label(1 "Public, In-State") label(2 "Private, In-State") ///
				label(3 "Out-of-State")) ///
				plabel(_all percent) 
				
				graph export "${concept_graphs}/enroll_system.png", replace

			restore
	}

	//Diagnostic Chart 2.b.1: Percentage Enrolled in College 1 Year after HS by Demographic/Academic Charactaristics
	{ 
	 foreach var in SEX RACE ENTER_VARS_HERE   {
		
			*1.Data set up
				preserve
					
				*keeping 1st year after HS
					keep if years_from_hs==1
				
				*setting up local and preserving value labels for the chart title 
					local val "`var'"
					
					local vtext : variable label `var' //creating locals to apply variable labels to graph
					if `"`vtext'"' == "" local vtext "`var'"
				
				*generating counter to get total in each category
					gen counter=1 
					
				*collapsing to get sum in each enrollment and demographic category 
					collapse (sum) counter, by(IN_COLLEGE_INDICATOR `var')
					bysort `var' : egen total=sum(counter)		
					
				*generating percent of students in each category
					gen pct_`var'=round((counter/total)*100)
						
				*keeping percent who enrolled in college
					keep if IN_COLLEGE_INDICATOR==1	
			

			*2. Graph
					graph bar pct_`var' , over(`var', sort(pct_)) blabel(total)  ///
						title("Percentage Enrolled in College 1 Year after HS" ///
						`"by `vtext'"') ///
						ytitle("Percentage Enrolled") ///
						 note ("SAMPLE INFO HERE", size(small))
						 
					graph export "${saved_graphs}/pct_enrolled_1yr_out_${var}.pdf", replace
				
				restore
			}
	}
				
	//Diagnostic Chart 2.b.2: Percentage Enrolled in College 1 Year after HS by Demographic Charactaristics and Test Score
	{
				preserve

				foreach var in RACE SEX ENTER_VARS_HERE {
					foreach var2 in MATH_TEST_QUARTILE  {
					
					*1. Data set up
						*keeping 1st year after HS 
							keep if years_from_hs==1
						
						*defining a local for labeling 
							local val "`var'"	  
						
						*setting up local and preserving value labels for the chart title
							local vtext : variable label `var' //creating locals to apply variable labels to graph
							if `"`vtext'"' == "" local vtext "`var'"
							
							local vtext2 : variable label `var2' //creating locals to apply variable labels to graph
							if `"`vtext2'"' == "" local vtext "`var2'"
						
						*generating counter to get total students in each category
							gen counter=1 
					
						*collapsing to get sum in each enrollment, demographic, and test score category 
							collapse (sum) counter, by(IN_COLLEGE_INDICATOR `var' `var2')
							bysort `var' `var2' : egen total=sum(counter)		
					
						*generating percent of students in each category
							gen pct_`var'=(counter/total)*100
						
						*keeping percent who enrolled in college
							keep if IN_COLLEGE_INDICATOR==1
							
						*percent of students in each quartile for markers, which are scaled to represent sample size
							bys `var' : egen total2=sum(IN_COLLEGE_INDICATOR)
							gen counter2=counter/total2
			
					*Graph
					if inlist("`val'", "RACE", "SEX", "BINARY VARIABLES HERE") {
							twoway (scatter pct_`var' `var2' if `var'==1 [fw=counter2], c(l) lp(solid) ms(Sh) mc(dkgreen) lc(dkgreen) msize(counter2)) ///
							(scatter pct_`var' `var2' if `var'==0 [fw=counter2], c(l) ms(Dh) lp(tight_dot) mc(sea) lc(sea) msize(counter2)), ///
							title("Percentage Enrolled in College 1 Year after HS" ///
							`"by `vtext' and `vtext2'"') ///
							ytitle("Percentage Enrolled") ///
							ysc(r(0(10)100)) ylabel(0(10)100) ymtick(0(10)100) ///
							legend(order(1 "`:label (`var') 1'" 2 "`:label (`var') 0'")) ///
							note("SAMPLE INFO HERE", size(vsmall))
					graph export "${saved_graphs}/prob_deg_attainment_${var1}_${var2}.pdf", replace
					restore
					}
						
					if inlist("`val'", "CATEGORICAL VARS WITH 3 CATEGORIES HERE" ){ //to add more categories, add another scatter statement and modify the `var' number, as well as the legend statement
							twoway (scatter pct_`var' `var2' if `var'==1 [fw=counter2], c(l) lp(dot) ms(Sh) mc(dkgreen) lc(dkgreen) msize(counter2)) ///
							(scatter pct_`var' `var2' if `var'==2 [fw=counter2], c(l) lp(dash) ms(Dh) mc(sea) lc(sea) msize(counter2)) ///
							(scatter pct_`var' `var2' if `var'==3 [fw=counter2], c(l) lp(shortdash) ms(Oh) mc(reddish) lc(reddish) msize(counter2)), ///
							title("Percentage Enrolled in College 1 Year after HS" ///
							`"by `vtext' and `vtext2'"') ///
							ytitle("Percentage Enrolled") ///
							ysc(r(0(10)100)) ylabel(0(10)100) ymtick(0(10)100) ///
							legend(order(1 "`:label (`var') 1'" 2 "`:label (`var') 2'" 3 "`:label (`var') 3'")) ///
							 note("SAMPLE INFO", size(vsmall)) 
						graph export "${saved_graphs}/prob_deg_attainment_${var1}_${var2}.pdf", replace
						restore
						}	
						}
						}	
			
	}	




