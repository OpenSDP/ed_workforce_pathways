/*******************************************************************************
	Section 1 Code: College Completion
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
	
	// If you do not already have the user-written Stata color scheme plotplainblind or the user-written sankey package installed, please see Stata's website for more guidance on 
	// how to install this. The appropriate installation approach might depend on the environment in which you are using Stata.
	

	
	/***************************************
	 Set up and setting filepaths
	***************************************/
	
	*version 17.0
	clear all
	macro drop all	

	ssc install blindschemes 
	set scheme plotplainblind //colorblind-friendly graph scheme
	
	ssc install sankey //install sankey program
	
	global path_to_folder "some_directory/perhaps_another_directory/maybe_even_one_more_directory/"
	global path_to_data	"$path_to_folder/data/sections_1_2_4.dta" // set path to your saved .csv data file matching spec document
	global saved_graphs		"$path_to_folder/graphs" // set path for where you'd like graphs saved on your computer
	
	
	/***************************************
	 Load data set up according to spec
	***************************************/
	
	*import delimited "${path_to_data}" //for CSV files
	use "${path_to_data}", clear //for .dta files
	
	/********************************************
	 Label variables as relevant to state
	********************************************/
	
	* female
		label define male_vals 0 "Male" 1 "Female" // set these value labels according to your preferred conventions, these are examples
		label values SEX male_vals
		label variable SEX "Sex"
	
	* race
		label define race_vals 1 "Asian" 2 "Black" 3 "Hispanic" 4 "White" 
		label values RACE race_vals
		label variable RACE "Race/Ethnicity"
		
	* degree
		label define degree_vals 1 "HS/GED" 2 "Certificate/Diploma" 3 "Associate" 4 "Bachelor's" 5 "Graduate"
		label values HIGHEST_DEGREE_IN_YEAR degree_vals
		label variable HIGHEST_DEGREE_IN_YEAR "Highest Degree in Year"
		
	* math test score
		label variable MATH_TEST_QUARTILE "Math Test Score Quartile"
		
	* year	
		label variable YEAR "Year"
	
	* status in year
		label define yes_no 1 "Yes" 0 "No"
		label values IN_COLLEGE_INDICATOR WORKING yes_no 
		
		label variable WORKING "Working"
		label variable IN_COLLEGE_INDICATOR "Enrolled in College"
	
	* recieved degree in HS
		label variable  DEG_IN_HS "Recieved Degree in HS"
		label values DEG_IN_HS yes_no 
	
	* enrollment information
		label variable SECTOR "Sector of Enrollment"
		
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

	//Diagnostic Chart 1.a.1: Percentage of Graduating Classes with Postsecondary Credentials
	{
			*1. Data set up
			preserve
				
				*generating counter to calculate the total N of students in each category
					gen counter=1
			
				*collapsing to get the sum of stuents in each credential category and year
					collapse (sum) counter, by (years_from_hs HIGHEST_DEGREE_IN_YEAR)
					bys years_from_hs : egen total=sum(counter)
			
				*generating the percent of students in each credential category
					gen pct_credential=counter/total
		
				*generating new variables representing percent in each credential category
					gen hs_pct=(pct_credential)*100 if HIGHEST_DEGREE_IN_YEAR==1 //high school diploma
					gen cd_pct=(pct_credential)*100 if HIGHEST_DEGREE_IN_YEAR==2 //certificate/diploma
					gen aa_pct=(pct_credential)*100 if HIGHEST_DEGREE_IN_YEAR==3 //associate
					gen ba_pct=(pct_credential)*100 if HIGHEST_DEGREE_IN_YEAR==4 //bachelor's degree
					gen grad_pct=(pct_credential)*100 if HIGHEST_DEGREE_IN_YEAR==5 //graduate degree

			*2. Graph
				graph bar cd_pct-grad_pct if inrange(years_from_hs,1,10), over(years_from_hs) stack ///
				title("Percentage of Graduating Classes with Postsecondary Credentials") ///
				ytitle("Percent of Classes") blabel("Years from HS Graduation") ///
				ysc(r(0(10)100)) ylabel(0(10)100) ymtick(0(10)100) ///
				legend(order(4 3 2 1) label(1 "Certificate or Diploma") label(2 "Associate") ///
				label(3 "Bachelors") label(4 "Graduate")) note("SAMPLE INFO HERE", size(small))
				
				*graph export "${saved_graphs}/enroll_over_time.png", replace
			restore
	}
		
	//Diagnostic Chart 1.a.2: Degree Attainment 10 Years from HS by Demographic/Academic Charactaristics
	{
			foreach var in SEX RACE MATH_TEST_QUARTILE {
				
				preserve
				
				*1. Data set up
					*setting up local and preserving value labels for the chart title
						local val "`var'"
					
						local vtext : variable label `var'
						if `"`vtext'"' == "" local vtext "`var'"	
					
					*keeping 10th year after HS- this can be modified based on the time horizon in which you want to measure attainment
						keep if years_from_hs==10
					
					*collapsing the sum of students by charactaristic and credential type
						gen counter=1 //generating counter so we can sum observations in each cateogry
						collapse (sum) counter, by(HIGHEST_DEGREE_IN_YEAR `var')
					
					*generating percent in each category
						bys `var' : egen `var'_count=sum(counter) //generating total counts of students within each demographic category
						
						 
						gen hs_pct=(counter/`var'_count)*100 if HIGHEST_DEGREE_IN_YEAR==1 //high school diploma
						gen cd_pct=(counter/`var'_count)*100 if  HIGHEST_DEGREE_IN_YEAR==2 //certificate/diploma
						gen aa_pct=(counter/`var'_count)*100 if HIGHEST_DEGREE_IN_YEAR==3 //associate
						gen ba_pct=(counter/`var'_count)*100 if HIGHEST_DEGREE_IN_YEAR==4 //bachelor's degree
						gen grad_pct=(counter/`var'_count)*100 if HIGHEST_DEGREE_IN_YEAR==5 //graduate degree
						
				*2. Graph
					graph bar cd_pct-grad_pct  , over(`var') stack  ///
					title("Degree Attainment 10 Years" `"from HS by `vtext'"') ///
					ytitle("Percent of Classes X")  ///
					ysc(r(0(10)100)) ylabel(0(10)100) ymtick(0(10)100) ///
					legend(label(4 "Graduate") label(3 "Bachelors") ///
					label(2 "Associate") label(1 "Certificate," "or Diploma") order(4 3 2 1))  ///
					note ("SAMPLE INFO" , size(small))
					
					graph export "${saved_graphs}/enroll_`var'.png", replace
				
				restore 
			}
	}
			
	//Diagnostic Chart 1.a.3: Percentage Earned Postsecondary Credential within 10 Years of HS by Student Demographics and Test Score
	{
		
			*Graph 
				foreach var in  RACE SEX {
					foreach var2 in MATH_TEST_QUARTILE  {
					
					preserve
						*keeping 10th year after HS- this can be modified based on the time horizon in which you want to measure attainment
							keep if years_from_hs==10			

						*generate outcome (whether a student has a college credential in the year measured)
							gen hasdegree=0 
							replace hasdegree=1 if HIGHEST_DEGREE_IN_YEAR>1 & HIGHEST_DEGREE_IN_YEAR~=. //1=HS/GED
						
						*setting up local and preserving value labels for the chart title
							local val "`var'"	
							local val2 "`var2'"
						
							local vtext : variable label `var' //creating locals to apply variable labels to graph
							if `"`vtext'"' == "" local vtext "`var'"
							
							local vtext2 : variable label `var2' //creating locals to apply variable labels to graph
							if `"`vtext2'"' == "" local vtext "`var2'"
						
						*generating counter to get total in each degree category
							gen counter=1 
					
						*collapsing to get sum in each degree, test, and demographic category  
							collapse (sum) counter, by(hasdegree `var' `var2')
							bysort `var' `var2' : egen total=sum(counter)		
					
						*generating percent in each degree category
							gen pct_`var'=(counter/total)*100
						
						*keeping percent who enrolled in college
							keep if hasdegree==1
							
						*percent of students in each quartile for weighted markers
							bys `var' : egen total2=sum(counter)
							gen counter2=counter/total2
			
					*Graph
					if inlist("`val'", "SEX", "BINARY VARIABLES HERE") {
							twoway (scatter pct_`var' `var2' if `var'==1 [fw=counter2], c(l) lp(solid) ms(Sh) mc(dkgreen) lc(dkgreen) msize(counter2)) ///
							(scatter pct_`var' `var2' if `var'==0 [fw=counter2], c(l) ms(Dh) lp(tight_dot) mc(sea) lc(sea) msize(counter2)), ///
							title("Percentage Earned Postsecondary Credential within 10 Years of HS" ///
							`"by `vtext' and `vtext2'"') ///
							ytitle("Percent") ///
							ysc(r(0(10)100)) ylabel(0(10)100) ymtick(0(10)100) ///
							legend(order(1 "`:label (`var') 1'" 2 "`:label (`var') 0'")) ///
							note("SAMPLE INFO" ///
							 "Size of markers notes relative sample size", size(vsmall))
							 
							 graph export "${saved_graphs}/prob_deg_attainment_`var'_`var2'.pdf", replace
					}
						
					if inlist("`val'", "RACE" )	{
							twoway (scatter pct_`var' `var2' if `var'==1 [fw=counter2], c(l) lp(dot) ms(Sh) mc(dkgreen) lc(dkgreen) msize(counter2)) ///
							(scatter pct_`var' `var2' if `var'==2 [fw=counter2], c(l) lp(dash) ms(Dh) mc(sea) lc(sea) msize(counter2)) ///
							(scatter pct_`var' `var2' if `var'==3 [fw=counter2], c(l) lp(shortdash) ms(Oh) mc(reddish) lc(reddish) msize(counter2)) ///
							(scatter pct_`var' `var2' if `var'==4 [fw=counter2], c(l) lp(shortdash_dot) ms(T) mc(vermillion) lc(vermillion) msize(counter2)) , ///
							title("Percentage Earned Postsecondary Credential within 10 Years of HS" ///
							`"by `vtext' and `vtext2'"') ///
							ytitle("Percent") ///
							ysc(r(0(10)100)) ylabel(0(10)100) ymtick(0(10)100) ///
							legend(order(1 "`:label (`var') 1'" 2 "`:label (`var') 2'" 3 "`:label (`var') 3'" 4 "`:label (`var') 4'")) ///
							 note("SAMPLE INFO" ///
							 "Size of markers notes relative sample size", size(vsmall)) 
							 
							 graph export "${saved_graphs}/prob_deg_attainment_`var'_`var2'.pdf", replace
					}
							
					
					if inlist("`val'", "INSERT VARS WITH 5 CATEGORIES") { 
							twoway (scatter pct_`var' `var2' if `var'==0 [fw=counter2], c(l) lp(dot) ms(Sh) mc(dkgreen) lc(dkgreen) msize(counter2)) ///
							(scatter pct_`var' `var2' if `var'==1 [fw=counter2], c(l) lp(dash) ms(Dh) mc(sea) lc(sea) msize(counter2)) ///
							(scatter pct_`var' `var2' if `var'==2 [fw=counter2], c(l) lp(shortdash) ms(Oh) mc(reddish) lc(reddish) msize(counter2)) ///
							(scatter pct_`var' `var2' if `var'==3 [fw=counter2], c(l) lp(shortdash_dot) ms(T) mc(vermillion) lc(vermillion) msize(counter2)) ///
							(scatter pct_`var' `var2' if `var'==4 [fw=counter2], c(l) lp(shortdash_dot) ms(T) mc(PII2) lc(PII2) msize(counter2)),  ///
							title("Percentage Earned Postsecondary Credential within 10 Years of HS" ///
							`"by `vtext' and `vtext2'"') ///
							ytitle("Percent") ///
							ysc(r(0(10)100)) ylabel(0(10)100) ymtick(0(10)100) ///
							legend(order(1 "`:label (`var') 0'" 2 "`:label (`var') 1'" 3 "`:label (`var') 2'" 4 "`:label (`var') 3'" 5 "`:label (`var') 4'")) ///
							 note("SAMPLE INFO" ///
							 "Size of markers notes relative sample size", size(vsmall)) 
							 
							 graph export "${saved_graphs}/prob_deg_attainment_`var'_`var2'.pdf", replace
						}
						}
						restore
						}
						
	}

	//Diagnostic Chart 1.a.4: Percentage of Graduating Classes who Completed, are Enrolled, Stopped Out, or Never Attempted College 10 Years from HS Graduation
	{ 
			*Data set up
				cap drop hasdegree*
				replace HIGHEST_DEGREE_IN_YEAR = 0 if mi(HIGHEST_DEGREE_IN_YEAR)
				preserve
					
				*keeping 1-10th year after HS- this can be modified based on what time horizon you want to measure attainment
					keep if inrange(years_from_hs, 1, 10)
					
				*generating flag for whether a student has a degree
					gen hasdegree=0
					replace hasdegree=1 if HIGHEST_DEGREE_IN_YEAR>1
				
				*1:generating percent of stuents who went to college 
				
					*generating ever enrolled flag
						bysort STUDENT_ID :  egen everenrolled= max(IN_COLLEGE_INDICATOR)
						
					*keep 10th year after HS graduation
						keep if years_from_hs==10
									
					*generating counter
						gen counter=1
						
					*saving n count as a local to generate percents of the whole population later 
						sum counter 
						local ncount=r(sum)
						di `ncount'
						
					*collapsing by everenrolled variable
						collapse (sum) counter, by(everenrolled)
						
					*cleaning for chart 
						egen total=sum(counter)
						gen percent=round((counter/total)*100) //generating percent who attended college
						
						gen source="Graduated from HS" //generating source variable for chart
						
						gen destination="Entered College" if everenrolled==1 //generating destination variable for chart
						replace destination="Never Entered College" if everenrolled==0
						
						gen layer=0 //gen layer variable for chart to help order sankey
						
						drop everenrolled 
					
					*save tempfile 
						tempfile master
						save `master'
						
						restore 
					
				*2: generating percent of college attempters who stopped out or never stopped out
						preserve 
						
						*keeping 10 years of data
							keep if inrange(years_from_hs, 1, 10)
						
						*ID students who recieved a degree in HS and drop them (doing so because we want to look at stop outs with no degree)
							drop if DEG_IN_HS==1
						
						*generating ever enrolled flag, keeping if ever enrolled
							bysort STUDENT_ID :  egen everenrolled= max(IN_COLLEGE_INDICATOR)
							keep if everenrolled==1
							drop everenrolled

						*only looking at stopout prior to any degree completion
							gen hasdegree=0
							replace hasdegree=1 if HIGHEST_DEGREE_IN_YEAR>1
							
							sort STUDENT_ID  HIGHEST_DEGREE_IN_YEAR years_from_hs //dropping years after degree was earned
							qui by STUDENT_ID  HIGHEST_DEGREE_IN_YEAR : gen dup = cond(_N==1,0,_n)
							drop if HIGHEST_DEGREE_IN_YEAR>1 & dup>1
							drop dup
							
							sort STUDENT_ID hasdegree years_from_hs //only keeping first degree
							qui by STUDENT_ID  hasdegree : gen dup = cond(_N==1,0,_n)
							drop if HIGHEST_DEGREE_IN_YEAR>1 & dup>1
							drop dup
							
							replace hasdegree=2 if hasdegree==1 //generating an indicator for use later
							replace hasdegree=. if hasdegree==0
					
						* Only keeping observations after first enrollment
							sort STUDENT_ID  IN_COLLEGE_INDICATOR years_from_hs //iding year of first enrollment
							qui by STUDENT_ID  IN_COLLEGE_INDICATOR : gen dup = cond(_N==1,0,_n)
							gen firstenroll=years_from_hs if IN_COLLEGE_INDICATOR==1 & dup<=1
							drop dup
							
							sort STUDENT_ID years_from_hs //applying year to all observations
							bysort STUDENT_ID : egen firstenrollyear=max(firstenroll)
							
							drop if years_from_hs<firstenrollyear //dropping before first enrollment
				
						*reshape 
							sort STUDENT_ID  years_from_hs
							keep STUDENT_ID years_from_hs IN_COLLEGE_INDICATOR hasdegree
							reshape wide IN_COLLEGE_INDICATOR hasdegree, i(STUDENT_ID) j(years_from_hs)
						
							order hasdegree1 hasdegree2 hasdegree3 hasdegree4 hasdegree5 ///
							hasdegree6 hasdegree7 hasdegree8 hasdegree9 hasdegree10, after(IN_COLLEGE_INDICATOR10) //ordering data so that years (represented by numbers in variable name) are in order 
							
							egen enrollpattern=concat(IN_COLLEGE_INDICATOR1-IN_COLLEGE_INDICATOR10) //generating variable with enrollment pattern 0=not enrolled, 1=enrolled. 
							egen gradpattern=concat(hasdegree1-hasdegree10) //generating variable with graduation pattern .=no degree, 2=has degree
							/* The data for enrollpatern will look something like this ..11011... 
							The data for grad pattern will look something like this ......2...
							Starting with enrollpattern, each value (whether period or integer) represents a student's enrollment status in a given year after high school.
							There are 10 values here, meaning we're looking at the 10 years after HS graduation.
							The missing indicators before the first "1" indicates that a student had not yet enrolled in college. In the example above,
							we see that student did not enroll in college until the third year after high school (represented by the 1 in position 3).
							We then see that the student was enrolled in years 3 and 4 after high school, stopped out in year 5 (indicated by the 0) and 
							re-enrolled in years 6 and 7. The missing indicators in positions 8-10  indicate that the student earned a their first credential
							in year 7, and we are not observing their movements after their first credential. We can confirm this by looking at gradpattern, which 
							shows a 2 in year 7 indicating that they recieved their first college credential in that year.
							*/
							*tab enrollpattern
							*tab gradpattern
							
						*ID stopouts using patterns in enrollpattern
							gen stopout=strpos(enrollpattern, "10") //1 meaning they were enrolled in a given year and 0 meaning they weren't enrolled the year after
							replace stopout=1 if stopout>0
							
						*generating counter
							gen counter=1 
							
						*collapsing and saving
							collapse (sum) counter, by(stopout)
							
						*cleaning for chart 
							gen percent=round((counter/`ncount')*100) //generating the percent of the total population who entered college and stopped out or never stopped out
							
							gen source="Entered College"
							
							gen destination="Stopped Out" if stopout==1
							replace destination="Never Stopped Out" if stopout==0
							
							gen layer=1
							
							drop stopout 
						
						*save tempfile 
							tempfile master2
							save `master2'
							
						restore 
					
				*3: generating percent who stopped out and their final outcome (completed, still enrolled, not enrolled)
						preserve 

						*keeping 10 years of data
						keep if inrange(years_from_hs, 0, 10)
						
						*ID students who recieved a degree in HS and drop them (doing so because we want to look at stop outs with no degree)
							drop if DEG_IN_HS==1					
						
						*generating ever enrolled flag, keeping if ever enrolled
							bysort STUDENT_ID :  egen everenrolled= max(IN_COLLEGE_INDICATOR)
							keep if everenrolled==1
							drop everenrolled

						*Only looking at stopout prior to any degree completion
							gen hasdegree=0
							replace hasdegree=1 if HIGHEST_DEGREE_IN_YEAR>1
							
							sort STUDENT_ID  HIGHEST_DEGREE_IN_YEAR years_from_hs //dropping years after degree was earned
							qui by STUDENT_ID  HIGHEST_DEGREE_IN_YEAR : gen dup = cond(_N==1,0,_n)
							drop if HIGHEST_DEGREE_IN_YEAR>1 & dup>1
							drop dup
							
							sort STUDENT_ID hasdegree  years_from_hs //only keeping first degree
							qui by STUDENT_ID  hasdegree : gen dup = cond(_N==1,0,_n)
							drop if HIGHEST_DEGREE_IN_YEAR>1 & dup>1
							drop dup
							
							replace hasdegree=2 if hasdegree==1 //generating an indicator for use later
							replace hasdegree=. if hasdegree==0
					
						* Only keeping observations after first enrollment
							sort STUDENT_ID  IN_COLLEGE_INDICATOR years_from_hs //iding year of first enrollment
							qui by STUDENT_ID  IN_COLLEGE_INDICATOR : gen dup = cond(_N==1,0,_n)
							gen firstenroll=years_from_hs if IN_COLLEGE_INDICATOR==1 & dup<=1
							drop dup
							
							sort STUDENT_ID years_from_hs //applying year to all observations
							bysort STUDENT_ID : egen firstenrollyear=max(firstenroll)
							
							drop if years_from_hs<firstenrollyear //dropping before first enrollment
				
						*reshape 
							sort STUDENT_ID  years_from_hs
							keep STUDENT_ID years_from_hs IN_COLLEGE_INDICATOR hasdegree
							reshape wide IN_COLLEGE_INDICATOR hasdegree, i(STUDENT_ID) j(years_from_hs)
						
							order hasdegree1 hasdegree2 hasdegree3 hasdegree4 hasdegree5 ///
							hasdegree6 hasdegree7 hasdegree8 hasdegree9 hasdegree10, after(IN_COLLEGE_INDICATOR10)
							
							egen enrollpattern=concat(IN_COLLEGE_INDICATOR1-IN_COLLEGE_INDICATOR10)
							egen gradpattern=concat(hasdegree1-hasdegree10)
						
						*ID stopouts
							gen stopout=strpos(enrollpattern, "10") //1 meaning they were enrolled in a given year and 0 meaning they weren't enrolled the year after
							replace stopout=1 if stopout>0
						
						*ID grads
							gen grad=strpos(gradpattern, "2") //2 meaning they recieved a degree in the time period we are examining
							replace grad=1 if grad>0 
							
						
						*ID still enrolled 
							gen enrolledsem=strrpos(enrollpattern, "1") //1 meaning they are still enrolled in the last year we are observing 
							gen enrolled=0 
							replace enrolled=1 if enrolledsem==10 & grad~=1
						
						*keeping stopouts
							keep if stopout==1
							
						*generating counter
							gen counter=1 
							
						*collapsing and saving
							collapse (sum) counter, by(grad enrolled)

						*cleaning for chart 
							egen complete=sum(counter) if grad==1 
							gen percent=round((counter/`ncount')*100) //generating the percent of the total population who stopped out and completed, are enrolled, or are no longer enrolled in year 10
							
							gen source="Stopped Out"
							
							gen destination="Enrolled" if enrolled==1 & grad==0
							replace destination="Completed" if grad==1
							replace destination="Not Enrolled" if grad==0 & enrolled==0
							
							drop if grad==1 & enrolled==1 
							
							gen layer=2
							
							drop grad enrolled complete 
						
						*save tempfile 
							tempfile master3
							save `master3'
							restore 

				*4: generating percent who never stopped out and their final outcome (completed, still enrolled, not enrolled)

						*keeping 10 years of data
						keep if inrange(years_from_hs, 0, 10)

						*ID students who recieved a degree in HS and drop them (doing so because we want to look at stop outs with no degree)
							drop if DEG_IN_HS==1
							
						*generating ever enrolled flag, keeping if ever enrolled
							bysort STUDENT_ID :  egen everenrolled= max(IN_COLLEGE_INDICATOR)
							keep if everenrolled==1
							drop everenrolled
						
						*Only looking at stopout prior to any degree completion
							gen hasdegree=0
							replace hasdegree=1 if HIGHEST_DEGREE_IN_YEAR>1
							
							sort STUDENT_ID  HIGHEST_DEGREE_IN_YEAR years_from_hs //dropping years after degree was earned
							qui by STUDENT_ID  HIGHEST_DEGREE_IN_YEAR : gen dup = cond(_N==1,0,_n)
							drop if HIGHEST_DEGREE_IN_YEAR>1 & dup>1
							drop dup
							
							sort STUDENT_ID hasdegree   years_from_hs //only keeping first degree
							qui by STUDENT_ID  hasdegree : gen dup = cond(_N==1,0,_n)
							drop if HIGHEST_DEGREE_IN_YEAR>1 & dup>1
							drop dup
							
							replace hasdegree=2 if hasdegree==1 //generating an indicator for use later
							replace hasdegree=. if hasdegree==0
					
						* Only keeping observations after first enrollment
							sort STUDENT_ID  IN_COLLEGE_INDICATOR years_from_hs //iding year of first enrollment
							qui by STUDENT_ID  IN_COLLEGE_INDICATOR : gen dup = cond(_N==1,0,_n)
							gen firstenroll=years_from_hs if IN_COLLEGE_INDICATOR==1 & dup<=1
							drop dup
							
							sort STUDENT_ID years_from_hs //applying year to all observations
							bysort STUDENT_ID : egen firstenrollyear=max(firstenroll)
							
							drop if years_from_hs<firstenrollyear //dropping before first enrollment
				
						*reshape 
							sort STUDENT_ID  years_from_hs
							keep STUDENT_ID years_from_hs IN_COLLEGE_INDICATOR hasdegree
							reshape wide IN_COLLEGE_INDICATOR hasdegree, i(STUDENT_ID) j(years_from_hs)
						
							order hasdegree1 hasdegree2 hasdegree3 hasdegree4 hasdegree5 ///
							hasdegree6 hasdegree7 hasdegree8 hasdegree9 hasdegree10, after(IN_COLLEGE_INDICATOR10)
							
							egen enrollpattern=concat(IN_COLLEGE_INDICATOR1-IN_COLLEGE_INDICATOR10)
							egen gradpattern=concat(hasdegree1-hasdegree10)
						
						*ID stopouts
							gen stopout=strpos(enrollpattern, "10") //1 meaning they were enrolled in a given year and 0 meaning they weren't enrolled the year after
							replace stopout=1 if stopout>0
						
						*ID grads
							gen grad=strpos(gradpattern, "2") //2 meaning they recieved a degree in the time period we are examining
							replace grad=1 if grad>0 
							
						
						*ID still enrolled 
							gen enrolledsem=strrpos(enrollpattern, "1") //1 meaning they are still enrolled in the last year we are observing 
							gen enrolled=0 
							replace enrolled=1 if enrolledsem==10 & grad~=1
						
						*keeping stopouts
							keep if stopout==0
							
						*generating counter
							gen counter=1 
							
						*collapsing and saving
							collapse (sum) counter, by(grad enrolled)

						*cleaning for chart 
							drop if grad==0 & enrolled==0
						
							gen percent=round((counter/`ncount')*100) //generating the percent of the total population who never stopped out and are still enrolled or completed in year 10
							
							gen source="Never Stopped Out"
							
							gen destination="Enrolled" if enrolled==1
							replace destination="Completed" if grad==1
							
							drop if grad==0 & enrolled==0
							
							gen layer=2
							
							drop grad enrolled  
					
							
				*5: appending datasets 
					append using `master' `master2' `master3'
					
				*6: graph 
					sankey percent , from(source) to(destination) by(layer) labs(2.7) laba(0) labpos(3) labg(.5) ///
					offset(5) noval showtot scheme(plotplainblind) alpha(60) ///
					title("Percentage of Graduating Classes who Completed, are Enrolled" "Stopped Out, or Never Attempted College 10 Years from HS Graduation" ///
					, size(4)) xsize(2.5) ysize(1) aspectratio(0.5) ///
					note("Sample includes class of X measured 10 years after HS graduation" ///
					"Stopout defined as students who enrolled in at least 1 semester of college" ///
					"and stopped out for at least 1 year prior to degree completion", ///
					size (small))
				
				graph export "${saved_graphs}/sankey'.png", replace
	}			

	/******************************************
	 Additional Charts
	******************************************		
	These graphs are designed to provide context to your analyses.
	For example, if you see that Back students in the first quartile of test scores
	are more likey to complete college than White students in the same quartile
	you may want to know how students are distributed across these quartiles
	by race/ethnicity. Graph 1 accomplishes this. Graph 2 shows the percent of students
	in each demographic category to help you understand the size of your populations of interest*/

	//Graph 1: Percentage of students in each demographic category by test score quartile
	{

				foreach var in RACE SEX  {
					foreach var2 in MATH_TEST_QUARTILE  {
					preserve
					
					*1. Data set up
						*keeping 1st year after HS 
							keep if years_from_hs==1
				
						*defining a local for if statements
							local val "`var'"

						*creating locals to apply variable labels to graph
							local vtext : variable label `var' 
							if `"`vtext'"' == "" local vtext "`var'"
							
							local vtext2 : variable label `var2' 
							if `"`vtext2'"' == "" local vtext "`var2'"
						
						*Generating counter for all observations and collapsing by predictor and test scores
							gen counter=1
						
							collapse (sum) counter, by(`var' `var2')
						
						*generating percent of students in each category
							drop if `var2'==.
							bysort `var' : egen total=sum(counter)
						
							gen pct=round((counter/total)*100)
						
						*Reshaping data for graph
							drop counter total
							reshape wide pct, i(`var') j(`var2')

					*2. Graph
							 graph bar pct*, over(`var', sort(order)) stack  legend(label (1 "Quartile 1") ///
								label (2 "Quartile 2") label (3 "Quartile 3") label(4 "Quartile 4") size(small)) ///
								title (`"Percent of `vtext2'"' `"by `vtext'"') ytitle("Percent") ///
								note ("SAMPLE INFO HERE", size(small)) ///
								ysc(r(0(25)100)) ylabel(0(25)100) ymtick(0(25)100)
							
							graph export "${saved_graphs}/pct_demos_${var1}_${var2}.pdf", replace
					restore
				 }
				 }	
		
	}

	//Graph 2:  Percentage of students in each demographic category
	{				
			foreach var in RACE SEX   {
						
					preserve
					*1. Data set up
							*keep years out
							keep if years_from_hs==1
							
							*defining a local for if statements
								local val "`var'"
					
							*creating locals to apply variable labels to graph
								local vtext : variable label `var' 
								if `"`vtext'"' == "" local vtext "`var'"						
							
							*generating counter and collapsing by variable
								gen counter=1
								collapse (sum) counter, by(`var')
					*2. Graph		
						graph pie counter, over(`var') plabel (_all percent) ///
							title(`"Percent of Graduates by `vtext'"') ///
							note ("SAMPLE INFO HERE", size(small))
						
						graph export "${saved_graphs}/pct_demos_${var1}.pdf", replace
				restore
					}
	}

	
	
	
