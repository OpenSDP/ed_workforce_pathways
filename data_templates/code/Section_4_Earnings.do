/*******************************************************************************
	Section 4 Code
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
		
	* degree
		label define degree_vals 1 "HS/GED" 2 "Certificate/Diploma" 3 "Associate" 4 "Bachelor's" 5 "Graduate"
		label values HIGHEST_DEGREE_IN_YEAR degree_vals
	
	/******************************************
	 Generate additional variables
	******************************************/
	
	*Generating indicator for N years out of high school 
		gen years_from_hs=.
		replace years_from_hs=YEAR-HS_GRADUATION_YEAR 
		//year_measured is the year in which you're measuring outcomes, hs_graduation_year is the year a student graduated from HS.		
	
	
	/******************************************
	 Example linear imputation for missing wages
	******************************************/
	/* In the technical guide, we recommended linearly imputing missing wages for those who are missing 1
	wage quarter. Below we provide example code. "wages_q1" through "wages_q4" are variables that contain wage 
	amounts for each quarter in the year you plan to measure wages.  
	*/
	
	*1. Begin by regressing the missing quarter on the non-missing quarters for those who have all four quarters present. You will then generate a variable with the predicted wage values from your regression. You will repeat this for each quarter, switching out the dependent variable each time. You will use these predicted values to fill in the missing wage quarter.
	
		reg wages_q1 wages_q2 wages_q3 wages_q4
		predict imputed_wages_q1 if wages_q1==. & wages_q2~=. & wages_q3~=. & wages_q4~=.
		
		reg  wages_q2 wages_q1 wages_q3 wages_q4 
		predict imputed_wages_q2 if if wages_q2==. & wages_q1~=. & wages_q3~=. & wages_q4~=.

		reg wages_q3 wages_q2 wages_q1 wages_q4
		predict imputed_wages_q3 if wages_q3==. & wages_q2~=. & wages_q1~=. & wages_q4~=.
		
		reg  wages_q4 wages_q1 wages_q3 wages_q1
		predict imputed_wages_q4 if if wages_q2==. & wages_q1~=. & wages_q3~=. & wages_q1~=.
		
		
	*2. Next you will replace the missing wages with imputed values
		foreach var in wages_q1-wages_q4 {
				replace `var'=imputed_`var' if  `var'==.
			}
			
	*3. Finally, you will sum the quarterly wages together to arrive at the annual wages
		gen annualwages=wages_q1+wages_q2+wages_q3+wages_q4
	

	/******************************************
	 Diagnostic Charts
	******************************************/	
	
	//Diagnostic Chart 4.a.1: Mean Wages by Highest Degree 10 Years After HS Graduation Benchmarked Against the Living Wage Threshold
	{
	preserve
			*1.Data Set Up
				
				*keeping 10th year after HS- this can be modified based on the time horizon in which you want to measure wages
					keep if years_from_hs==10
					
				*collapse to mean wages by highest degree (can change to median by editing option next to collapse command)
					collapse (mean) ADJ_ANNUAL_WAGES,  by(HIGHEST_DEGREE_IN_YEAR)
			
			*2.Graph
				graph bar ADJ_ANNUAL_WAGES, over(HIGHEST_DEGREE_IN_YEAR) blabel(total, format(%9.0f)) ///
					ysc(r(0(10000)80000)) ymtick(0(10000)80000) ylabel(0(10000)80000) /// 
					title("Mean Wages by Highest Degree 10 Years After HS Graduation" ///
					"Benchmarked Against the Living Wage Threshold") ///
					legend(label(1 "Mean Wages")) ///
					 ytitle("Adjusted Annual Wages ($)") ylabel(, format(%9.0fc)) ///
					yline(insert living wage here, e.g. 30000) ///
					note("Wages adjusted to 2022 dollars. Wages for HS Classes of 2010 and 2011" ///
					"Dotted line is the average living wage between 20xx and 20yy adjusted to 20yy dollars" ///
					"Living wage estimates from MIT Living Wage Calculator" ///
					"Sample comprises all individuals with 3 or 4 wage quarters observed in 10th year after HS graduation, N = " ///
					"Missing quarter imputed for individuals with only 3 wage quarters observed", size(tiny))
					
			graph export "${saved_graphs}/mean_raw.pdf", replace
			restore		
	}

	//Diagnostic Chart 4.a.1: Distribution of Wages by Highest Degree 10 Years After HS Graduation Benchmarked Against the Living Wage Threshold
	{	

			graph twoway histogram ADJ_ANNUAL_WAGES,  fcolor(white%0) by(HIGHEST_DEGREE_IN_YEAR, row(1) ///
				title("Distribution of Wages by Highest Degree 10 Years After HS Graduation" "Benchmarked Against the Living Wage Threshold") ///
				note("Wages adjusted to 2022" "Wages for HS Classes of x" ///
				"Dotted line is the average living wage between 2020 and 2022 adjusted to 2022 dollars" ///
				"Living wage estimates from MIT Living Wage Calculator" ///
				"Sample comprises all individuals with 3 or 4 wage quarters observed in 10th year after HS graduation, N =  96,513" ///
				"Missing quarter imputed for individuals with only 3 wage quarters observed", size(tiny))) ///
				 bin(10) percent xline(insert living wage here, e.g. 30000)  ///
				 xtitle("Adjusted Annual Wages") 
				 
			
			graph export "${saved_graphs}/distribution.pdf", replace
			
	}	

	//Diagnostic Chart 4.a.3: Mean Wages by Highest Degree and by Demographic/Academic Charactaristics 10 Years After HS Graduation Benchmarked Against the Living Wage Threshold
	{
			foreach var in SEX RACE PROGRAM_OF_STUDY ENTER_VARS_HERE {
	preserve
			*1.Data set up	
				*keeping 10 years out from HS
					keep if years_out==10

				*setting up local and preserving value labels for the chart title
					local val "`var'"
					
					local vtext : variable label `var'
					if `"`vtext'"' == "" local vtext "`var'"
				
				*collapse to mean wages by highest degree and demographic charactaristic (can change to median by editing option next to collapse command)
					collapse (mean) ADJ_ANNUAL_WAGES,  by(HIGHEST_DEGREE_IN_YEAR `var')

			
			*2.Graph
				graph bar ADJ_ANNUAL_WAGES, over(HIGHEST_DEGREE_IN_YEAR) over(`var') ///
					nofill blabel(total, format(%9.0f) size(vsmall) orientation(vertical)) ///
					asyvar ysc(r(0(10000)80000)) ymtick(0(10000)80000) ylabel(0(10000)80000) /// 
					title(`"Mean Wages by Highest Degree and by `vtext'"' "10 Years After HS Graduation Benchmarked Against the Living Wage Threshold") ///
					 ytitle("Adjusted Annual Wages ($)", height(6)) ylabel(, format(%9.0fc)) ///
					yline(insert living wage here, e.g. 30000) ///
					note("Wages adjusted to 2022 dollars" "Wages for HS Classes of x" ///
					"Dotted line is the average living wage between 20xx and 20yy adjusted to 20yy dollars" ///
					"Living wage estimates from MIT Living Wage Calculator" ///
					"Sample comprises all individuals with 3 or 4 wage quarters observed in 10th year after HS graduation, N =" ///
					"Missing quarter imputed for individuals with only 3 wage quarters observed" ///
					"Wages winsorized at top and bottom 1st percentiles", size(tiny))
					
			graph export "${saved_graphs}/median_`var'.pdf", replace
			restore			
	}
	}

	//Diagnostic Chart 4.a.3: Mean Wages by Highest Degree and by Demographic Charactaristics and Test Score 10 Years After HS Graduation Benchmarked Against the Living Wage Threshold
	{
				foreach var in RACE SEX PROGRAM_OF_STUDY ENTER_VARS_HERE {
					foreach var2 in MATH_TEST_QUARTILE  {
	preserve
			*1. Data set up
				*setting up local and preserving value labels for the chart title 
					local val "`var'"
					
					local vtext : variable label `var' 
					if `"`vtext'"' == "" local vtext "`var'"	
					
				*keeping 10 years out from HS
					keep if years_out==10
					
				*keeping those in the first and fourth test quartile- you can change this based on your context
					keep if inlist(`var2', 1,4)

				*collapse to mean wages by highest degree, demographic charactaristic, and test score quartile (can change to median by editing option next to collapse command)
					collapse (mean) ADJ_ANNUAL_WAGES,  by(HIGHEST_DEGREE_IN_YEAR `var' `var2')

			*2. Graph
				graph bar ADJ_ANNUAL_WAGES, over(HIGHEST_DEGREE_IN_YEAR) over(`var') over(`var2', label(labsize(small))) ///
					asyvar ysc(r(0(10000)80000)) ymtick(0(10000)80000) ylabel(0(10000)80000) /// 
					title(`"Mean Wages by Highest Degree and by `vtext'"' "10 Years After HS Graduation Benchmarked Against the Living Wage Threshold") ///
					 ytitle("Adjusted Annual Wages ($)", height(6)) ylabel(, format(%9.0fc)) ///
					yline(insert living wage here, e.g. 30000) ///
					note("Wages adjusted to 2022 dollars" "Wages for HS Classes of x" ///
					"Dotted line is the average living wage between 20xx and 20yy adjusted to 20yy dollars" ///
					"Living wage estimates from MIT Living Wage Calculator" ///
					"Sample comprises all individuals with 3 or 4 wage quarters observed in 10th year after HS graduation, N =  96,513" ///
					"Missing quarter imputed for individuals with only 3 wage quarters observed" ///
					"Wages winsorized at top and bottom 1st percentiles", size(tiny))
				
			graph export "${saved_graphs}/mean_`var'_test.pdf", replace
			restore			
	}
			}
	}

	