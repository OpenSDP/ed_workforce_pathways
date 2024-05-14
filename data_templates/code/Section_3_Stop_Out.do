/*******************************************************************************
	Section 3 Code
*******************************************************************************/

	/*****************************************
	 Comments about this code
	*****************************************/
	
	// Data for this analysis code should be set up according to the accompanying data specification guide.
	
	// Code below for this analyis should be modified according to your needs. In some places, comments explicitly prompt
	// you to appropriately modify a minimum number of commands for your state's data, or else the output may not be
	// correct or useful. And of course feel free to modify any other commands, too.
	
	// Stata version 17 was used to generate the graphs below. If commands to not run, you will need to upgrade your Stata verion.
	
	// The code below requires a different data structure than the code in other sections of this diagnostic 
	//see the technical guide for data specifications.
	
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

	/******************************************
	 Diagnostic Charts
	******************************************/			
	
	//Diagnostic Chart 3.a.1: Percentage of Postsecondary Attempters by Enrollment Status 6 Years of College Entry by Demographic/Academic Charactaristics
	
	{	
			foreach var in SEX RACE ENTER_VARS_HERE    {
			preserve
			
				*1: Data set up	
					*dropping those who completed a degree in high school 
						drop if DEG_IN_HS==1
						
					*setting up local and preserving value labels for the chart title
						local val "`var'"
						
						local vtext : variable label `var'  
						if `"`vtext'"' == "" local vtext "`var'			
					
					*collapse data to get the number of students in each category 
						collapse (sum) STOPOUT DEGREE_COMPLETER CURRENTLY_ENROLLED if OUTCOME_MEASURED==1, by(`var') 
						//OUTCOME_MEASURED is an idicator for whether the stuent should be included in the analysis. See technical narrative for more details

					*generating the percent of students in each category
						egen total=rowtotal(STOPOUT DEGREE_COMPLETER CURRENTLY_ENROLLED) //generating total N of students
						
						foreach var2 in STOPOUT DEGREE_COMPLETER CURRENTLY_ENROLLED {
							gen pct_`var2'=(`var2'/total)*100
							}
					
					*prepare variables for chart	
						gen pct_stopout_neg=-1*pct_stopout //this allows data to go below 0 on the x axis
						
						label variable pct_stopout_neg "Stopped Out, No Degree" 
						label variable pct_degreecompleter "Postsecondary Completer" 
						label variable pct_currentlyenrolled "Enrolled, No Degree"
				
				*2. Graph	
					//note: change the title for this graph if you are measuring enrollment status at a time other than 6 years from college entry
						graph bar pct_stopout_neg pct_currentlyenrolled pct_degreecompleter, over(`var') stack ///
							title("Percentage of Postsecondary Attempters by Enrollment Status" ///
							`"6 Years of College Entry by `vtext'"', size(mediumsmall)) ///
							ysc(r(-100(20)100)) ylabel(-100 "100" -80 "80" -60 "60" -40 "40" ///
							-20 "20" 0 "0" 20 "20" 40 "40" 60 "60" 80 "80" 100 "100" ) ///
							yline(0, lcolor(gray)) ///
							legend (order(2 1 3)) ///
							bar(3, color(black)) bar(2, color(sky)) bar(1, color(gs10)) ///
							ytitle("Percentage of Postsecondary Attempters") ///
							note("Sample includes the HS classes of X measured at 6 years after college entry (X% of attempters)" ///
							"Stopout defined as students who have enrolled in at least 1 semester of college" ///
							"have not completed a degree, and was not enrolled in the year measured.", ///
							size (vsmall))
						
						graph export "${saved_graphs}/stopout_`var'.png", replace
			restore
				}
	}
				
	//Diagnostic Chart 3.a.2: Percentage of Postsecondary Attempters who Stopped Out by Demographic Charactaristics and Test Score
	{
				preserve
				
				foreach var in RACE SEX ENTER_VARS_HERE {
					foreach var2 in MATH_TEST_QUARTILE  {
				
				*1.Data set up
					*dropping those who completed a degree in high school 
						drop if DEG_IN_HS==1
							
					*setting up local and preserving value labels for the chart title
						local val "`var'"	  
							
						local vtext : variable label `var' 
						if `"`vtext'"' == "" local vtext "`var'"
								
						local vtext2 : variable label `var2' 
						if `"`vtext2'"' == "" local vtext "`var2'"
				
				
					*collapse data to get the number of students in each category 
						collapse (sum) STOPOUT DEGREE_COMPLETER CURRENTLY_ENROLLED if OUTCOME_MEASURED==1, by(`var' `var2')				

					*gen percent of stuents that stopped out
						egen total=rowtotal(STOPOUT DEGREE_COMPLETER CURRENTLY_ENROLLED) //generating total N of students
							
						gen pct_stopout=(STOPOUT/total)*100
						
					*percent of students in each quartile for markers
						bys `var' : egen total2=sum(stopout)
						gen counter2=stopout/total2
								
			
				*2.Graph
				//note: change the title for this graph if you are measuring enrollment status at a time other than 6 years from college entry
					if inlist("`val'", "SEX", "RACE", "BINARY PREDICTORS") {
							twoway (scatter pct_stopout `var2' if `var'==1 [fw=counter2], c(l) lp(solid) ms(Sh) mc(dkgreen) lc(dkgreen) msize(counter2)) ///
							(scatter pct_stopout `var2' if `var'==0 [fw=counter2], c(l) ms(Dh) lp(tight_dot) mc(sea) lc(sea) msize(counter2)), ///
							title("Percentage of Postsecondary Attempters who Stopped Out" ///
							`"by `vtext' and `vtext2'"') ///
							ytitle("Percentage of Postsecondary Attempters") ///
							ysc(r(0(10)100)) ylabel(0(10)100) ymtick(0(10)100) ///
							legend(order(1 "`:label (`var') 1'" 2 "`:label (`var') 0'")) ///
							note("SAMPLE INFO" ///
							 "Size of markers notes relative sample size", size(vsmall))
							 
							 graph export "${saved_graphs}/prob_stopout_${var}_{var2}.pdf", replace
					}
						
					if inlist("`val'", "PREDICTORS WITH 4 CATEGORIES" )	{
							twoway (scatter pct_stopout `var2' if `var'==1 [fw=counter2], c(l) lp(dot) ms(Sh) mc(dkgreen) lc(dkgreen) msize(counter2)) ///
							(scatter pct_stopout `var2' if `var'==2 [fw=counter2], c(l) lp(dash) ms(Dh) mc(sea) lc(sea) msize(counter2)) ///
							(scatter pct_stopout `var2' if `var'==3 [fw=counter2], c(l) lp(shortdash) ms(Oh) mc(reddish) lc(reddish) msize(counter2)) ///
							(scatter pct_stopout `var2' if `var'==4 [fw=counter2], c(l) lp(shortdash_dot) ms(T) mc(vermillion) lc(vermillion) msize(counter2)) , ///
							title("Percentage of Postsecondary Attempters who Stopped Out" ///
							`"by `vtext' and `vtext2'"') ///
							ytitle("Percentage of Postsecondary Attempters") ///
							ysc(r(0(10)100)) ylabel(0(10)100) ymtick(0(10)100) ///
							legend(order(1 "`:label (`var') 1'" 2 "`:label (`var') 2'" 3 "`:label (`var') 3'" 4 "`:label (`var') 4'")) ///
							note("SAMPLE INFO" ///
							 "Size of markers notes relative sample size", size(vsmall)) 
							 
							 graph export "${saved_graphs}/prob_stopout_${var}_{var2}.pdf", replace
						}				
						}
						}
					restore	
			
	}	

	//Diagnostic Chart 3.a.3: Percentage of Degree Attempters who Stopped Out by Number of Credits Earned in First Semester
	{
		preserve
			*1. Data Cleaning 
				*dropping those who completed a degree in high school 
						drop if DEG_IN_HS==1
						
				*keeping those who enrolled with at least 6 years to complete (this can change depending on your context, you may want to measure within 3 years of entry if you are looking at sub-BA attemtpers)
						keep if OUTCOME_MEASURED==1
						
				*keeping those who attempted a certain degree in the first term 
						keep if ATTEMPTED_DEGREE_FIRST_TERM	=="Bachelors"
						
				*identifying those who stopped out within 6 years of postsecondary entry by subtracting the stop out term from a students' first term
						gen years_between=STOP_OUT_TERM-FIRST_TERM
						
						gen stopout=0 if STOP_OUT_TERM==. //these are students who never stopped out 
						replace stopout=1 if years_between<=6 //students who stopped out within 6 years of entry. The number here will also depend on how your data are structured (i.e. whether your data use calendar year or school year). You will change this if you are looking at stop out within another timeframe
						drop if stopout==. //dropping those whose stop out status is unclear (if applicable)
					
			*2. Prep data for graph
					*binning hours
						keep if inrange(CREDIT_HOURS_FIRST_TERM, 0, 18) //0-99th pctile, this will change based on your data
				
						gen bin=.
						replace bin=0 if CREDIT_HOURS_FIRST_TERM==0
						replace bin=3 if inrange(CREDIT_HOURS_FIRST_TERM, 1, 3)
						replace bin=6 if inrange(CREDIT_HOURS_FIRST_TERM, 4, 6)
						replace bin=9 if inrange(CREDIT_HOURS_FIRST_TERM, 7, 9)
						replace bin=12 if inrange(CREDIT_HOURS_FIRST_TERM, 10, 12)
						replace bin=15 if inrange(CREDIT_HOURS_FIRST_TERM, 13, 15)
						replace bin=18 if inrange(CREDIT_HOURS_FIRST_TERM, 16, 18)

					*collapse data by bin and stop out
						gen counter=1
						
						collapse (sum) counter, by(bin stopout)
						drop if bin==.
						
					*generating total in each bin
						bys bin  : egen total=total(counter) 
						
					*generating percent who stopped out in each bin	
						gen pct=(counter/total)*100 
					
					*label data
						label variable bin "Credits"
					
			*3. Graph
						twoway (connected pct bin if stopout==1), ///
						 ytitle("Percentage") ///
						 title("Percentage of  Degree Attempters who Stopped Out by Number of" ///
						 "Credits Earned in First Semester") ///
						 xtitle("Credits Earned in First Term") ///
						 xlabel( 1 "0" 2 "1-3" 3 "4-6" 4 "7-9" 5 "10-12" 6 "13-15" 7 "16-18" ) ///
						 ysc(r(0(10)100)) ylabel(0(10)100) ymtick(0(10)100) ///
						 xsc(r(0(3)10)) xlabel(0(3)18) xmtick(0(3)18) ///
						 note("Credits earned in the first term are grouped into three-credit intervals," ///
						 "labeled by the maximum of the interval" /// 
						 "Sample includes X measured at X years after college entry (X% of attempters)" ///
						 "Stopout defined as students who have enrolled in at least 1 semester of college" ///
						"have not completed a degree, and were not enrolled in the year measured" ///
						"Credits include those earned at all institutions in which a student was enrolled", ///
						size (vsmall))
						
						graph export "${saved_graphs}/stopout_credaccum_BA.png", replace
					
	}

	//Diagnostic Chart 3.a.3:  Percentage of Degree Attempters who Stopped Out by GPA Earned in First Semester 
	{
			preserve
			*1. Data Cleaning 
				
				*dropping those who completed a degree in high school 
						drop if DEG_IN_HS==1
						
				*keeping those who enrolled with at least 6 years to complete (this can change depending on your context, you may want to measure within 3 years of entry if you are looking at sub-BA attemtpers)
						keep if OUTCOME_MEASURED==1
						
				*keeping those who attempted a certain degree in the first term
						keep if ATTEMPTED_DEGREE_FIRST_TERM	=="Bachelors"
						
				*identifying those who stopped out within 6 years of postsecondary entry by subtracting the stop out term from a students' first term
						gen years_between=STOP_OUT_TERM-FIRST_TERM
						
						gen stopout=0 if STOP_OUT_TERM==. //these are students who never stopped out 
						replace stopout=1 if years_between<=6 //students who stopped out within 6 years of entry. The number here will also depend on how your data are structured (i.e. whether your data use calendar year or school year). You will change this if you are looking at stop out within another timeframe
						drop if stopout==. //dropping those whose stop out status is unclear (if applicable)
			
			*2. Prep for graph
					
					*generate bins for GPA
						gen gpa_bin=.
						replace gpa_bin=1 if GPA_FIRST_TERM==0
						replace gpa_bin=2 if GPA_FIRST_TERM>0 & GPA_FIRST_TERM<1
						replace gpa_bin=3 if GPA_FIRST_TERM>=1 & GPA_FIRST_TERM<2
						replace gpa_bin=4 if GPA_FIRST_TERM>=2 & GPA_FIRST_TERM<3
						replace gpa_bin=5 if GPA_FIRST_TERM>=3 & GPA_FIRST_TERM<4
						replace gpa_bin=6 if GPA_FIRST_TERM==4

					*collapse data by bin and stop out 
						gen counter=1
						
						collapse (sum) counter, by(gpa_bin stopout)
						drop if gpa_bin==.
						
					*generating total in each bin
						bys bin  : egen total=total(counter) 
						
					*generating percent who stopped out in each bin	
						gen pct=(counter/total)*100 
					
					*label data
						label variable gpa_bin "Cumulative GPA"
					
			*3. Graph
						
						twoway (connected pct gpa_bin if stopout==1), ///
						 ytitle("Percentage") ///
						 title("Percentage of Degree Attempters who Stopped Out" "by GPA in First Semester") ///
						 xtitle("GPA") ysc(r(0(10)100)) ylabel(0(10)100) ymtick(0(10)100) ///
						 xsc(r(1(1)5)) xlabel(1(1)5) xmtick(1(1)5) ///
						 xlabel(1 "0" 2 ">0-.99" 3 "1.0-1.99" 4 "2.0-2.99" 5 "3.0-3.99" 6 "4.0" ) ///
						 note("Sample includes class of X measured at X years after college entry (X% of attempters)" ///
						 "Stopout defined as students who have enrolled in at least 1 semester of college" ///
						"have not completed a degree, and were not enrolled in the year measured" ///
						"Credits include those earned at all institutions in which a student was enrolled", ///
						size (vsmall))
						
						graph export "${saved_graphs}/stopout_gpa_BA.png", replace
						
	}

	//Diagnostic Chart 3.a.4: Percentage of Stop Outs by Number Credits Away from Degree Minimums
	{
			preserve
			
			*1. Data Cleaning 
				*dropping those who completed a degree in high school 
						drop if DEG_IN_HS==1
						
				*keeping those who enrolled with at least 6 years to complete (this can change depending on your context, you may want to measure within 3 years of entry if you are looking at sub-BA attemtpers)
						keep if OUTCOME_MEASURED==1
						
				*keeping those who attempted a certain degree in the last semester prior to stop out
						keep if ATTEMPTED_DEGREE_STOP_OUT	=="Bachelors"
						
				*identifying those who stopped out within 6 years of postsecondary entry by subtracting the stop out term from a students' first term
						gen years_between=STOP_OUT_TERM-FIRST_TERM
						
						gen stopout=0 if STOP_OUT_TERM==. //these are students who never stopped out 
						replace stopout=1 if years_between<=6 //students who stopped out within 6 years of entry. The number here will also depend on how your data are structured (i.e. whether your data use calendar year or school year). You will change this if you are looking at stop out within another timeframe
						drop if stopout==. //dropping those whose stop out status is unclear (if applicable)
						
						keep if stopout==1 //keeping stop outs
				
			*2. Prep for chart
				
					*generate credit minimums by degree, for example, the minimum N credits neeed to earn a BA may be 120 
						generate deg_minimum=.
						replace deg_minimum=[insert number here] //this will vary based on your context
						
					*generate the number of credits a student was away from the degree minimum at first stop out
						generate credits_away=deg_minimum-CREDIT_HOURS_CUM
						
					*generate bins for credits
						gen bin=.
						replace bin=1 if credits_away==0
						replace bin=2 if inrange(credits_away, 1, 14)
						replace bin=3 if inrange(credits_away, 15, 29)
						replace bin=4 if inrange(credits_away, 30, 44)
						replace bin=5 if inrange(credits_away, 45, 59)
						replace bin=6 if inrange(credits_away, 60, 74)
						replace bin=7 if inrange(credits_away, 75, 89)
						replace bin=8 if inrange(credits_away, 90, 104)
						replace bin=9 if credits_away>=105 & credits_away~=.
						
					*prepare for collapse
						gen counter=1
						
					*collapse data by bin
						collapse (sum) counter, by(bin)
						
					*generate percent of students in each category
						egen total=total(counter) //generating total N of stopouts
						gen pct=round((counter/total)*100) //generating percent of stopouts in each bin
							
					*label bins	
						label define label 1 "0" 2 "1-14" 3 "15-29" 4 "30-44" 5 "45-59" 6 "60-74" 7 "75-89" 8 "90-104" 9 ">105"
						label values bin label
							
						label variable bin "Credits"
						
			*3.Graph	
						graph bar pct, over(bin) blabel(total) ///
						ytitle("Percentage of Stop Outs") ///
						title ("Percentage of Stop Outs by Number Credits Away from ATTEMPTED_DEGREE_STOP_OUT Degree Minimum") ///
						 ysc(r(0(10)100)) ylabel(0(10)100) ymtick(0(10)100) ///
						note("Stopout defined as students who enrolled in at least 1 semester of college" ///
						"had not completed a degree, and was not enrolled at year X" ///
						"Credits include those earned at all institutions in which a student was enrolled" ///
						"SAMPLE INFO", ///
						size (vsmall))
						
						graph export "${saved_graphs}/stopout_degmins_BA.png", replace
			restore
	}

	//Diagnostic Chart 3.b.1 : Percentage of Stop Outs Who Re-Enrolled Within Five Years of Departure
	{
		preserve	
			
			*1. Data Cleaning 
				
				*dropping those who completed a degree in high school 
						drop if DEG_IN_HS==1
						
				*keeping those who enrolled with at least 6 years to complete (this can change depending on your context, you may want to measure within 3 years of entry if you are looking at sub-BA attempters)
						keep if OUTCOME_MEASURED==1
										
				*identifying those who stopped out within 6 years of postsecondary entry by subtracting the stop out term from a students' first term
						gen years_between=STOP_OUT_TERM-FIRST_TERM
						
						gen stopout=0 if STOP_OUT_TERM==. //these are students who never stopped out 
						replace stopout=1 if years_between<=6 //students who stopped out within 6 years of entry. The number here will also depend on how your data are structured (i.e. whether your data use calendar year or school year). You will change this if you are looking at stop out within another timeframe
						drop if stopout==. //dropping those whose stop out status is unclear (if applicable)
						
						keep if stopout==1 //keeping stop outs
					
				*keeping those who had at least 5 years to re-enroll after stop out. You will accomplish this by subtracting 5 (or however long you'd like to give students to re enter) from the last term avaialble in your data
						gen last_term=[insert last term measured in data e.g. spring 2020]-5
						keep if STOP_OUT_TERM<=last_term //keeping if students stopped out prior to 5 years before the end of the panel
						
				*generating time between stop out and re-enrollment 
						gen time_between=REENROLL_SEMESTER-STOP_OUT_TERM
						replace time_between=0 if REENROLL_SEMESTER==. //this is students who stopped out and never re-enrolled
				
					
			*2. Prep data for graph			
					*gen counter 
						gen counter=1 
					
					*collapsing sum of stop outs by time elapsed between stop out and re-enrollment 
						collapse (sum) counter, by(time_between)
										
						egen total=sum(counter)
				
					*replace counter to equal the sum of those who never re-enrolled or re-enrolled after 5 years
						egen total2=sum(counter) if time_between==0 | time_between>5
						replace counter=total2 if time_between==0
						drop if time_between>5
					
					*generating percent of stopouts who came back after x years
						gen pct_between=round((counter/total)*100)
					
					*adding row for sum of enrollment
						set obs `=_N+2'
						replace time_between=6 in `=_N-1' 
						//the time_between value will change based on how long you are 
						//allowing students to re-enroll. Here we're looking at 5 years, 
						//so the additional row counting the sum will get a 6.
						replace time_between=7 in `=_N'
						sum pct_between if time_between~=0
						replace pct_between=`r(sum)' in `=_N-1'
						sum pct_between if time_between==0
						replace pct_between=`r(sum)' in `=_N'
						
						replace time_between=8 if time_between==0 
						//the time_between value will change based on how long 
						//you are allowing students to re-enroll.
						sort time_between
						gen id=_n
						order id
				
					*generating groups to categorize the type of bar we want to create
						cap drop group
						gen group=.
						replace group=1 if time_between==1 | id==`=_N'-2 //first and last sum of re-enrolled bars
						replace group=2 if pct_between >=0 & group==. //middle bars only showing the additional re-enrollees
						replace group=3 if id==`=_N'-1 //sum of non-reenrollers
						replace group=4 if total2~=. //last bar with total enrolled vs. not enrolled

						cap drop hi lo //this code is establishing the height of the bars
						gen lo=.
						gen hi=.
						replace lo=0 if group==1 //bar 1 starts at 0
						replace hi=pct_between if group==1 //bar 1 ends at the percent enrolled in the first year out
						
						replace hi=100 if group==3 //this bar starts at the max % re-enrolled and shows the % not reenrolled
						replace lo=pct_between[_n-1] if group==3

						forval i=2/`=_N-2' {
							replace lo=hi[_n-1] in `i' if group==2 //this code makes it so the bars showing the additional percent enrolled over the prior year start at the highest point of the prior year's bar instead of zero
							replace hi=lo+pct_between in `i' if group==2 //this establishes the height of the bar 
						}
									
					*generating x axis labels and spacing them apart
						gen x=.
						replace x=id*2
						label define yrla 2 "One" 4 "Two" ///
						6 "Three" 8 "Four" 10 "Five" ///
						12 "Sum of Re-Enrolled" 14 "Sum of Not Re-Enrolled" 16 "Total Stop Outs" //this will change based on the number of years you're allowing students to re-enroll
						
						label values x yrla
				
					*adding leading lines to go between bars
						gen li_start=x
						gen li_end=x[_n+1] if group==1 | group==2

						cap drop pos
						gen pos=1 if group==1 | group==3
						
					
					*setting up labels
						cap drop ylab 
						gen ylab=.

						replace ylab=hi+7 
						
					*preparing for last stacked bar 
						gen pct_between2=100-pct_between if id==`=_N'
						gen lo2=pct_between2 if id==`=_N'
						gen hi2=100 if id==`=_N'
					

			*3. Graph
						twoway ///
						(rbar lo hi x if group==2 , fi(80) fc(black) barwidth(1)) ///
						(rbar lo hi x if group==3, fi(80) fc(gs12) barwidth(1)) ///
						(rbar lo hi x if group==1, fi(80) fc(black) barwidth(1)) ///
						(bar pct_between2 x if group==4, fi(80) fc(black) barwidth(1)) ///
						|| (rbar lo2 hi2 x if group==4, fi(80) fc(gs12) barwidth(1)) ///
						(pcspike hi li_start hi li_end, lc(black) lw(0.08)) ///
						(scatter ylab x, mlab(pct_between) mlabs(2.5) mlabpos(6) mlabangle(90) mc(none))  ///
						, legend(order(4 5) label(4 "Re-Enrolled") label(5 "Not Re-Enrolled")) ///
						xlabel(2(2)16, valuelabel labsize(vsmall) angle(45)) ///
						title("Percentage of Stop Outs Who Re-Enrolled Within Five Years of Departure") ///
						ytitle("Percentage of Stop Outs") ///
						xtitle("Years Since Stop Out") ///
						note("Sample includes classes of x who stopped out within 6 years of college entry," ///
						"and had 5 years to re-enter. Stop outs defined as those who were absent from enrollment" ///
						"for at least one year prior to earning their first credential", ///
						size(vsmall))
						
						graph export "${saved_graphs}/stopout_reenroll.png", replace
		restore

		}	

