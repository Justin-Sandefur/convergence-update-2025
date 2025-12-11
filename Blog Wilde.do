******************
*** BLOG WILDE ***
******************

	*** Load data 
		drop _all
		clear matrix
		clear mata
		set maxvar 10000
		use "Output/combined_data_2025update.dta", clear 
		capture drop _merge
		drop *_pop
		drop if year < 1960
	
	*** Define income groups
		foreach data in mad pwt wdi {
			summ `data' if ccode == "USA" & year == 1960 
			local usa = `r(mean)'
			summ pwt if year == 1960, det 
			local lowthresh = `r(p25)'/`usa'
			local highthresh = `r(p75)'/`usa'
			gen us = `data' if ccode == "USA"
			sort year us
			by year: replace us = us[_n-1] if missing(us) & !missing(us[_n-1])
			gen catl = `lowthresh'*us
			gen catu = `highthresh'*us
			gen cat`data' = "H" if `data' >= catu & !missing(`data')
			replace cat`data' = "M" if `data' < catu & `data' >=catl & !missing(`data')
			replace cat`data' = "L" if `data' < catl 
			drop us catl catu 
		}
		keep mad pwt wdi catmad catpwt catwdi ccode country year 	
		sort ccode
		by ccode: replace country = country[_n-1] if !missing(country[_n-1])
		reshape wide wdi pwt mad catmad catpwt catwdi, i(ccode country) j(year)
		*drop if pwt1990==.|wdi1990==.|mad1990==.
		
	*** Add in region names
		gen g7 = inlist(ccode, "USA", "CAN", "JPN", "ITA", "GBR", "FRA", "DEU")
		kountry ccode, from(iso3c) geo(undet)
		gen clusters = "West" if country == "Kosovo" | regexm(GEO, "Europe")==1 | regexm(GEO, "Australia and New Zealand") | inlist(country, "Canada", "United States")
		replace clusters = "Africa" if regexm(GEO, "Africa")==1
		replace clusters = "Latin America" if  GEO == "South America" | GEO == "Caribbean" | GEO == "Central America" 
		replace clusters = "Asia" if regexm(GEO, "Asia")==1 | GEO == "Melanesia" | GEO == "Micronesia" | GEO == "Polynesia" | country == "Taiwan"
		/*
		preserve
			capture wbopendata, indicator(sp.pop.totl ) clear
			rename countrycode ccode
			keep ccode adminregionname 
			duplicates drop 
			tempfile wdi
			save `wdi', replace
		restore
		merge 1:1 ccode using `wdi'
		drop if inlist(ccode, "AIA", "MSR")
		drop if _merge == 2
		drop _merge
		gen continent = subinstr(adminregionname, "(excluding high income)","",1)
		replace continent = "High Income" if missing(continent)        
		replace continent = strtrim(continent)
		*/
		tab clusters, gen(continent_)
		
	*** Cycle through regressions
		local j = 1	
		foreach data in wdi pwt  mad {			
			// Customize window by dataset
			local firstyear = 1960
			if "`data'" == "pwt" {
				local lastyear = 2023 //2019
			} 
			if "`data'" == "wdi" {
				local lastyear = 2023
			}
			if "`data'" == "mad" {
				local lastyear = 2023 //2022
			}	
			local endyear = `lastyear' - 1 		
				forval startyear = `firstyear'(1)`endyear' {
					local startplus1 = `startyear' + 1
					forval outcomeyear = `startplus1'(1)`lastyear' {
						gen outcome = (log(`data'`outcomeyear'/`data'`startyear')/(`outcomeyear' - `startyear'))
						egen g7_ = mean(outcome) if g7 == 1
						egen g7g = max(g7_)
						qui sum g7g 
						local g7mean = `r(mean)'
						drop g7_ 
						gen share_ = outcome > g7g if !missing(outcome) & cat`data'`startyear' != "H" 
						gen shareM_ = outcome > g7g if !missing(outcome) & cat`data'`startyear' == "M" 
						gen shareL_ = outcome > g7g if !missing(outcome) & cat`data'`startyear' == "L" 
						egen shareM = mean(shareM_)
						egen shareL = mean(shareL_)					
						ci proportion share_
						local ciprop = `r(mean)'
						local cilb = `r(lb)'
						local ciub = `r(ub)'
						egen share = mean(share_)
						forval i = 1/4 {
							egen countcont_`i' = total(continent_`i') if share_ == 1
							gen share`i'_ = outcome > g7g if !missing(outcome) & cat`data'`startyear' != "H" & continent_`i' == 1
							egen share`i' = mean(share`i'_)
						}
						*** Preserving this for disaggregated graph
							if `outcomeyear' - `startyear'==10{
								gen converging_`data'_`outcomeyear' = -1 + 2*share_
							}
						foreach var in share shareL shareM {
							drop `var'_ 
							qui sum `var'
							local `var'mean = `r(mean)'
						}	
						gen gap_ = outcome - g7g if  !missing(outcome) & cat`data'`startyear' != "H" 
						egen gap = mean(gap_)
						forval i = 1/4 {
							gen gap`i'_ = outcome - g7g if  !missing(outcome) & cat`data'`startyear' != "H" & continent_`i' == 1
							gen condgap`i'_ = outcome - g7g if !missing(outcome) & cat`data'`startyear' != "H" & continent_`i' == 1 & (outcome > g7g) & !missing(outcome)
							egen condgap`i' = mean(condgap`i'_)
							egen gap`i' = mean(gap`i'_)
							drop gap`i'_ 
							qui sum gap`i' 
							if `r(N)' > 0 {
								local gap`i' = `r(mean)'
							}
								else {
									local gap`i' = -999
								}
							qui sum condgap`i' 
							if `r(N)' > 0 {
								local condgap`i' = `r(mean)'
							}
								else {
									local condgap`i' = -999
								}
						}
						drop gap_ 
						qui sum gap 
						local gapmean = `r(mean)'
						gen condgap_ = outcome - g7g if  !missing(outcome) & cat`data'`startyear' != "H" & (outcome > g7g) & !missing(outcome)
						egen condgap = mean(condgap_)
						drop condgap_ 
						qui sum condgap 
						local condgapmean = `r(mean)'
						forval i = 1/4 {
							qui sum countcont_`i'
							local countcont`i' = `r(mean)'
							qui sum share`i' 
							if `r(N)' > 0 {
								local share`i' = `r(mean)'
							}
								else {
									local share`i' = 0
								}
						}
						preserve
							clear
							set obs 1
							tempfile file`j'
							gen measure = "`data'"
							gen share = `sharemean'
							gen shareL = `shareLmean'
							gen shareM = `shareMmean'
							gen g7 = `g7mean'
							gen gap = `gapmean'
							gen ciprop = `ciprop'
							gen cilb = `cilb'
							gen ciub = `ciub'
							gen condgap = `condgapmean'
							gen startyear = `startyear'
							gen endyear = `outcomeyear'
							forval i = 1/4 {
								gen countcont_`i' = `countcont`i''
								gen share`i' = `share`i''
								gen gap`i' = `gap`i''
								gen condgap`i' = `condgap`i''
							}
							save `file`j''
						restore
						drop outcome g7g share* gap* condgap* countcont_* 
						local ++ j
					}
				}
		}
		
		* Combine results
		clear
		local jminus1 = `j' - 1
		forval i = 1/`jminus1' {
			append using `file`i''
		}
		gen period = endyear - startyear
		bys measure: egen maxend = max(endyear)
		rename countcont_1 africa
		rename countcont_2 asia 
		rename countcont_3 latinamerica
		rename countcont_4 west 
		rename share1 shareafrica 
		rename share2 shareasia 
		rename share3 sharelatinamerica
		rename share4 sharewest 
		forval i = 1/4 {
			replace gap`i' = . if gap`i' == -999
			replace condgap`i' = . if condgap`i' == -999
		}
		rename gap1 gapafrica 
		rename gap2 gapasia 
		rename gap3 gaplatinamerica
		rename gap4 gapwest 
		rename condgap1 condgapafrica 
		rename condgap2 condgapasia 
		rename condgap3 condgaplatinamerica
		rename condgap4 condgapwest 
		save "Output/wilde_stats.dta", replace
	 
		
		use "Output/wilde_stats.dta", clear 
				
		
		global hcol #064D49 
		global mcol #509EF0 // #B2CFED
		global lcol #0B1525 // #FFCBED 
		global bcol #F6F6F6 // #C8D8D5 // #ECECEC 
		
		global pwtcol #064D49 
		global wdicol #509EF0 
		global madcol #0B1525 
		gen startyear2 = startyear + .2
		gen startyear3 = startyear + .4
	
	*** Text wrapping for figure note
		local MyNote = "The figure plots the unweighted share of countries whose growth rate exceeds the unweighted average growth rate of the G7 countries in a given period for three data series: Maddison, the Penn World Tables 10.0, and the World Development Indicators. For PWT, missing data after 2019 is imputed using WDI growth rates. The starting year spans 1960 to 2015. The sample excludes oil exporters and countries with populations under one million."
		local l = length("`MyNote'")
		local b = ceil(`l'/100)

		if `l' > 110 {
			forv i = 1/`b' {
				local lab`i': piece `i' 100 of "`MyNote'", nobreak
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

		#delimit ;
			tw 	(rcap cilb ciub startyear if period == 10 & measure == "mad",  lcolor(${madcol}%30))  
				(rcap cilb ciub startyear2 if period == 10 & measure == "wdi",  lcolor(${wdicol}%30)) 
				(rcap cilb ciub startyear3 if period == 10 & measure == "pwt",  lcolor(${pwtcol}%30)) 
				(sc ciprop startyear if period == 10 & measure == "mad", mcolor(${madcol}%70)) 
				(sc ciprop startyear3 if period == 10 & measure == "pwt", mcolor(${pwtcol}%70 msymbol(D))) 
				(sc ciprop startyear2 if period == 10 & measure == "wdi", mcolor(${wdicol}%70 msymbol(S)))
				, ytitle("") 
				title("An alternative metric of catch-up growth", margin(b+1) placement(w) justification(left) span size(*1.1) color(${lcol})) 
				subtitle("Share of low- and middle-income countries with higher""growth rates than the G7", margin(b+3) placement(w) justification(left) span) 
				legend(order(4 "Maddison" 5 "PWT" 6 "WDI") bmargin(small) ring(0) pos(10) region(color(${bcol}))) 
				ylabel(, nogrid) 
				xlabel(1960 "1960-70" 1965 "1965-75" 1970 "1970-80" 1975 "1975-85" 1980 "1980-90" 1985 "1985-95" 1990 "1990-00" 1995 "1995-05" 2000 "2000-10" 2005 "2005-15" 2010 "2010-20" 2015 "2015-23", angle(45)) 
				xsize(4) ysize(5)
				note(`MyNote2', span size(vsmall))
				graphregion(color(${bcol}))
				plotregion(color(${bcol}))
				;
		#delimit cr
		graph export "Output/wilde_2025update.png", replace width(4000) height(5000)
		
		
exit

