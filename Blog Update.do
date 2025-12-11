*****************************
*** CONVERGENCE DATA PREP ***
*****************************

	*** Load Maddison data which can be downloaded from: https://www.rug.nl/ggdc/historicaldevelopment/maddison/
		tempfile maddison
		use "input/mpd2020.dta", clear
		rename (countrycode country gdppc pop) (ccode country mad mad_pop)
		drop if year <1950
		*** Drop former countries
			drop if ccode == "PRI" // Puerto Rico
			drop if ccode == "SUN" // Former USSR
			drop if ccode == "CSK" // Czechoslovakia
			drop if ccode == "YUG" // Former Yugoslavia
		replace mad_pop = mad_pop/1000
		save `maddison'
		
	*** Load WDI data which can be downloaded from: http://databank.worldbank.org/data/source/world-development-indicators
		tempfile wdi
		import delimited "input/wdi_2025_data.csv", clear
		forval i = 5/69 {
			local year = `i' + 1955
			rename v`i' value`year'
		}	
		drop if _n == 1
		drop if missing(v2)
		destring value*, force replace
		replace v3 = "pop" if v3 == "Population, total"
		replace v3 = "gdpppp" if regexm(v3,"PPP")==1
		replace v3 = "gdpconst" if regexm(v3,"2015")==1
		replace v3 = "gdpconstlcu" if regexm(v3, "LCU")==1
		rename (v1 v2) (country ccode)
		drop v4
		reshape long value, i(country ccode v3) j(year)
		reshape wide value, i(country ccode year) j(v3) string
		rename value* *
		drop if missing(gdpppp) & missing(gdpconst) 
		sort ccode year
		// For years prior to when PPP data is available, apply growth rates from constant national dollars retroactively 
		by ccode: gen gdpconstgrowth = gdpconst/gdpconst[_n-1]
		gsort ccode - year
		replace gdpppp = gdpppp[_n - 1]/gdpconstgrowth[_n - 1] if missing(gdpppp) & !missing(gdpppp[_n - 1]) & !missing(gdpconst) & !missing(gdpconst[_n - 1])
		rename (gdpconst gdpppp pop) (wdi_const wdi wdi_pop)
		replace wdi_pop = wdi_pop/1000000
		drop gdpconstgrowth wdi_const
		save `wdi'
		
	*** Load PWT data which can be downloaded from: https://www.rug.nl/ggdc/productivity/pwt/	
		use "input/pwt110.dta", clear
		rename countrycode ccode
		keep country ccode year pop rgdpe 
		rename (pop rgdpe) (pwt_pop pwt)
		foreach var in pwt {
			replace `var' = `var'/pwt_pop
		}
		drop if missing(pwt_pop)
		
	*** Add in Maddison and WDI
		merge 1:1 ccode year using `maddison'
		drop _merge 
		merge 1:1 ccode year using `wdi'
		drop _merge

	*** Oil-producers from IMF (http://datahelp.imf.org/knowledgebase/articles/516096-which-countries-comprise-export-earnings-fuel-a)
		gen oil = ccode == "DZA" | ///
			ccode == "AGO" | ///
			ccode == "AZE" | ///
			ccode == "BHR" | ///
			ccode == "BRN" | ///
			ccode == "TCD" | ///
			ccode == "COG" | ///
			ccode == "ECU" | ///
			ccode == "GNQ" | ///
			ccode == "GAB" | ///
			ccode == "IRN" | ///
			ccode == "IRQ" | ///
			ccode == "KAZ" | ///
			ccode == "KWT" | ///
			ccode == "NGA" | ///
			ccode == "OMN" | ///
			ccode == "QAT" | ///
			ccode == "RUS" | ///
			ccode == "SAU" | ///
			ccode == "TTO" | ///
			ccode == "TKM" | ///
			ccode == "ARE" | ///
			ccode == "VEN" | ///
			ccode == "YEM" | ///
			ccode == "LBY" | ///
			ccode == "TLS" | ///
			ccode == "SDN"
		gen nooil = 1-oil
		
	*** Small countries
		foreach data in mad pwt wdi {
			gen big_`data' = `data'_pop>=1 if !missing(`data'_pop)
		}
			
	*** Restrict sample
		foreach var in mad pwt wdi {
			replace `var' = . if big_`var'==0
		}
		drop if oil==1
		keep ccode country year pwt wdi mad *pop* 
				
	*** Apply WDI growth rates to missing years from PWT and Maddison
		sort ccode year
		by ccode: replace pwt = wdi[_n]/wdi[_n-1]*pwt[_n-1] if year > 2010 & missing(pwt)
		by ccode: replace mad = wdi[_n]/wdi[_n-1]*mad[_n-1] if year > 2010 & missing(mad)
		save "Output/combined_data_2025update.dta", replace	
		
		use "Output/combined_data_2025update.dta", clear 
		capture drop _merge
		drop *_pop
		drop if year < 1960
		reshape wide wdi pwt mad, i(ccode country) j(year)
		
	*** Cycle through regressions
		local j = 1	
		foreach data in wdi pwt mad {			
			local firstyear = 1960
			local lastyear = 2024 
			local endyear = `lastyear' - 1 		
				forval startyear = `firstyear'(1)`endyear' {
					local startplus1 = `startyear' + 1
					forval outcomeyear = `startplus1'(1)`lastyear' {
						gen outcome = (log(`data'`outcomeyear'/`data'`startyear')/(`outcomeyear' - `startyear'))
						gen initial = log(`data'`startyear')
						qui nl (outcome = {b0=1} - (1 - exp(-1*{b1=0.00}*(`outcomeyear' - `startyear')))/(`outcomeyear' - `startyear')*initial), vce(robust)  
						preserve
							clear
							set obs 1
							tempfile file`j'
							gen measure = "`data'"
							gen beta = _b[/b1]
							gen se = _se[/b1]
							gen lower = _b[/b1] - invttail(`e(df_r)',0.025)*_se[/b1]
							gen upper = _b[/b1] + invttail(`e(df_r)',0.025)*_se[/b1]
							gen tstat = _b[/b1]/_se[/b1]
							gen pval =2*ttail(`e(df_r)',abs(tstat))
							gen n = `e(N)'
							gen startyear = `startyear'
							gen endyear = `outcomeyear'
							save `file`j''
						restore
						drop outcome initial
						local ++ j
					}
				}
		}
	
	*** Combine results
		clear
		local jminus1 = `j' - 1
		forval i = 1/`jminus1' {
			append using `file`i''
		}
		gen period = endyear - startyear
		bys measure: egen maxend = max(endyear)
		save "Output/coefficients_2025update.dta", replace 
		