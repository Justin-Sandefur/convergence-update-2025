 /*
	Create 3 graphs, each showing statistics from a 
	range of starting years, through to end of PWT in 2019.
	First is average growth, second is volatility (SD), and
	third is persistence (growth corr in adjacent years).
*/


	use "Output/combined_data_2025update.dta", clear
	
*** Define income groups
	local data mad
	summ `data' if ccode == "USA" & year == 1960 
	local usa = `r(mean)'
	summ `data' if year == 1960, det 
	local lowthresh = `r(p25)'/`usa'
	local highthresh = `r(p75)'/`usa'
	gen us = `data' if ccode == "USA"
	sort year us
	by year: replace us = us[_n-1] if missing(us) & !missing(us[_n-1])
	gen catl = `lowthresh'*us
	gen catu = `highthresh'*us
	gen cat = "H" if `data' >= catu & !missing(`data')
	replace cat = "M" if `data' < catu & `data' >=catl & !missing(`data')
	replace cat = "L" if `data' < catl 
			
*** Time set
	egen ccodenum = group(ccode)
	tsset ccodenum year
	gen logy = log(`data')
	gen g1 = (log(`data'/l1.`data'))*100
	gen sd = .
	gen corr = .

*** Step 1. Create country-level statistics over time

*** Growth
	gen finalyear_ = `data' if year == 2023
	bys ccode: egen finalyear = max(finalyear_)
	drop finalyear_
	gen growth = (log(finalyear/`data')/(2023- year))*100

*** Volatility and persistence
	levelsof ccode, local(countries)
	forval year = 1960/2015 {
		foreach country of local countries {
			qui count if !missing(g1) & ccode == "`country'" & year == `year'
			if `r(N)' == 1 {
				summ g1 if ccode == "`country'" & year >= `year'
				replace sd = `r(sd)' if year == `year' & ccode == "`country'"	
				sort ccodenum year
				gen temp = l1.g1
				corr g1 temp if ccode == "`country'" & year >= `year'
				replace corr = `r(rho)' if year == `year' & ccode == "`country'"
				drop temp 
			}
		}
	}
		

*** Step 2. Aggregate those results to country income-groups 

	foreach type in H M L{
		gen mean_`type' = .
		gen sd_`type' = .
		gen lb_`type' = .
		gen ub_`type' = .
		gen corr_`type' = .
		gen corrlb_`type' = .
		gen corrub_`type' = .
		gen p95_`type' = .
		gen p5_`type' = .
	}

	forval year = 1960/2015 {
		foreach type in H M L {	
		
		*** Averages
			*summ growth if cat == "`type'" & year >= `year', det
			ci means growth if cat == "`type'" & year == `year'
			replace mean_`type' = `r(mean)' if year == `year'
			*replace p95_`type'= `r(p95)' if year == `year'
			*replace p5_`type'= `r(p5)' if year == `year'
			replace p95_`type'= `r(lb)' if year == `year'
			replace p5_`type'= `r(ub)' if year == `year'

		*** Volatility
			ci means sd if cat == "`type'" & year == `year'
			replace sd_`type' = `r(mean)' if cat == "`type'" & year == `year'
			replace lb_`type' = `r(lb)' if cat == "`type'" & year == `year'
			replace ub_`type' = `r(ub)' if cat == "`type'" & year == `year'
			
		*** Persistence
			ci means corr if cat == "`type'" & year == `year'
			replace corr_`type' = `r(mean)' if cat == "`type'" & year == `year'
			replace corrlb_`type' = `r(lb)' if cat == "`type'" & year == `year'
			replace corrub_`type' = `r(ub)' if cat == "`type'" & year == `year'
		}
	}
	
	gen year2 =year + .2
	gen year3 = year + .4
	
	save "Output/blogupdate_for_triptych.dta", replace

*** Step 3. Make graphs

	use "Output/blogupdate_for_triptych.dta", clear

	global hcol #064D49 
	global mcol #509EF0 // #B2CFED
	global lcol #0B1525 // #FFCBED 
	global bcol #F6F6F6 // #C8D8D5 // #ECECEC 
	
	keep year* *_H *_M *_L 
	duplicates drop
	foreach var of varlist *_H *_L *_M {
		sort year `var'
		by year: replace `var' = `var'[_n-1] if missing(`var') & !missing(`var'[_n-1])
	}
	duplicates drop
	
	*** Text wrapping for figure note
		local max = 100
		local MyNote = "The figure plots the unweighted average growth rate over 10-year perionds using the Maddison data for countries in each income category. Income categories are redefined each period, using the threshold of 25 and 75 percent of US income for low- and middle-income countries, respectively. The sample excludes oil exporters and countries with populations under one million."
		local l = length("`MyNote'")
		local b = ceil(`l'/`max') + 1
				
		if `l' > `max' {
			forv i = 1/`b' {
				local lab`i': piece `i' `max' of "`MyNote'", nobreak
				if `i'==1 {
					local MyNote2 `""`lab`i''""'
				}
				if `i'>=2 {
					local MyNote2 `"`MyNote2' "`lab`i''""'
				}
			}
		}
		else {
			local MyNote2 `""`MyNote'""'
		}

		di `"`MyNote2'"'	
		
		graph set window fontface "Times New Roman"

	*** Averages
		#delimit ;
		tw 	(rcap p95_H p5_H year if inrange(year, 1960, 2020),  lcolor(${hcol}%30)) 
			(rcap p95_M p5_M year2 if inrange(year, 1960, 2020), lcolor(${mcol}%30)) 
			(rcap p95_L p5_L year3 if inrange(year, 1960, 2020), lcolor(${lcol}%30)) 
			(sc mean_H year if inrange(year, 1960, 2020), mcolor(${hcol}%70)) 
			(sc mean_M year2 if inrange(year, 1960, 2020), msymbol(D) mcolor(${mcol}%70)) 
			(sc mean_L year3 if inrange(year, 1960, 2020), msymbol(S) mcolor(${lcol}%70))
			,
			legend(cols(3) order(4 "High income" 5 "Middle income" 6 "Low income") region(color(none)) bmargin(small) ring(1) pos(6)) 
			plotregion(style(none) lcolor(none))  
			xlabel(1960 "1960-70" 1965 "1965-75" 1970 "1970-80" 1975 "1975-85" 1980 "1980-90" 1985 "1985-95" 1990 "1990-00" 1995 "1995-05" 2000 "2000-10" 2005 "2005-15" 2010 "2010-20" 2015 "2015-23", angle(45)) 
			ylabel(1 "1%" 2 "2%" 3 "3%", glcolor(gs10))
			graphregion(fcol(white) lcol(white)) 
			title("Average growth rates by income group", margin(b+1) placement(w) justification(left) span size(*1.1) color(${lcol}))
			subtitle(`"Where income groups are defined as countries with less"'`"than 25% and 75% of US income, respectively"', margin(b+3) placement(w) justification(left) span)
			xsize(4) ysize(5) 
			note(`MyNote2', span size(vsmall))
			graphregion(color(${bcol}))
		;
		#delimit cr
		
		graph export "Output/blogupdate_growthdist_hml_sc.png", as(png) replace width(4000) height(5000)

exit

