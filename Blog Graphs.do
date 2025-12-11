
	*** Graphs 
		use "Output/coefficients_2025update.dta", clear 
		global hcol plb1 
		global mcol black
		global lcol plg1 
		
		global pwtcol plr1 
		global wdicol ply1 
		global madcol pll1 
		
			
	*** Rolling convergence graph
		keep if endyear == maxend
		
		*** For flourish
			preserve
				keep startyear measure beta lower upper
				reshape wide beta lower upper, i(startyear) j(measure) string
				rename *mad mad_*
				rename *wdi wdi_*
				rename *pwt pwt_*
				outsheet using "Output/coefficients.csv", comma replace
			restore

	*** Stagger years for graph
		gen startyear2 = startyear + .2
		gen startyear3 = startyear + .4
	
		#delimit ;
		tw  (rcap lower upper startyear if measure == "mad" & startyear >=1960 & startyear <= 2018, lcolor(${madcol}%30)) 	
			(rcap lower upper startyear2 if measure == "pwt" & startyear >=1960 & startyear <= 2018, lcolor(${pwtcol}%30)) 	
			(rcap lower upper startyear3 if measure == "wdi" & startyear >=1960 & startyear <= 2018, lcolor(${wdicol}%30)) 	
			(sc beta startyear if measure == "mad" & startyear >=1960 & startyear <= 2018, mcolor(${madcol}%70))	
			(sc beta startyear2 if measure == "pwt" & startyear >=1960 & startyear <= 2018, mcolor(${pwtcol}%70) msymbol(D))
			(sc beta startyear3 if measure == "wdi" & startyear >=1960 & startyear <= 2018, mcolor(${wdicol}%70) msymbol(S)), 	
			plotregion(style(none) lcolor(none)) yline(0,lcolor(black) lpattern(shortdash) lwidth(medium)) xlabel(1960(5)2015, angle(45)) 
				graphregion(fcol(white) lcol(white)) 
				ytitle("{&beta}", orientation(horizontal) size(large)) 
				xtitle("Initial Year") xsize(4) ysize(4)  ylabel(-.005(.0025).01, angle(horizontal))
				legend(order(	4 "Maddison" 
								5 "PWT"
								6 "WDI") pos(10) ring(0) col(1)
								region(lcolor(none) fcolor(none)));
		#delimit cr
		*graph export "Output/rollingbeta_transformed_by_series_2025update.png", replace
		
		use "Output/coefficients_2025update.dta", clear 
		global hcol #064D49 
		global mcol #509EF0 // #B2CFED
		global lcol #0B1525 // #FFCBED 
		global bcol #F6F6F6 // #C8D8D5 // #ECECEC 
		
		global pwtcol #064D49 
		global wdicol #509EF0 
		global madcol #0B1525 
	
	*** Constant period convergence graph
		keep if period== 10
		
	*** Stagger years for graph
		gen startyear2 = startyear + .2
		gen startyear3 = startyear + .4
	

	*** Text wrapping for figure note
		local max = 100
		local MyNote = "The figure plots the {&beta} parameter of unconditional convergence for three separate data series: Maddison, the Penn World Tables 10.0, and the World Development Indicators. For PWT, missing data after 2019 is imputed using WDI growth rates. Each coeffcient comes from a non-linear least squares regression following the specification in Patel et al (2021) of real per capita GDP PPP growth rate over 10 years on the initial per capita income. The starting year spans 1960 to 2015. The sample excludes oil exporters and countries with populations under one million."
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
		
		#delimit ;
		tw  (rcap lower upper startyear if measure == "mad" & startyear >=1960 & startyear <= 2018, lcolor(${madcol}%30)) 	
			(rcap lower upper startyear2 if measure == "pwt" & startyear >=1960 & startyear <= 2018, lcolor(${pwtcol}%30)) 	
			(rcap lower upper startyear3 if measure == "wdi" & startyear >=1960 & startyear <= 2018, lcolor(${wdicol}%30)) 	
			(sc beta startyear if measure == "mad" & startyear >=1960 & startyear <= 2018, mcolor(${madcol}%70))	
			(sc beta startyear2 if measure == "pwt" & startyear >=1960 & startyear <= 2018, mcolor(${pwtcol}%70) msymbol(D))
			(sc beta startyear3 if measure == "wdi" & startyear >=1960 & startyear <= 2018, mcolor(${wdicol}%70) msymbol(S)), 	
			yline(0,lcolor(black) lpattern(shortdash) lwidth(medium)) 
			ylabel(, nogrid)
			//plotregion(margin(b+6))
			title("{bf:{stSans:The rise and fall of global}}""{bf:{stSans:convergence}}", margin(b+1) placement(w) justification(left) span size(*1.2))
			subtitle(`"Unconditional "Solow" convergence over rolling 10-year"'`"periods"', margin(b+3) placement(w) justification(left) span)
			ytitle("{&beta}", orientation(horizontal) size(large)) 
			xsize(4) ysize(5)  
			ylabel(-.02(.005).01, angle(horizontal))
			xlabel(1960 "1960-70" 1965 "1965-75" 1970 "1970-80" 1975 "1975-85" 1980 "1980-90" 1985 "1985-95" 1990 "1990-00" 1995 "1995-05" 2000 "2000-10" 2005 "2005-15" 2010 "2010-20" 2015 "2015-23", angle(45)) 
			legend(order(	4 "Maddison" 
							5 "PWT"
							6 "WDI") pos(10) ring(0) col(1)
							region(lcolor(none) fcolor(none)))
			note(`MyNote2', span size(vsmall))
			graphregion(color(${bcol}))
			plotregion(color(${bcol}))
			;
		#delimit cr
		graph export "Output/beta10yr_transformed_by_series_2025update.png", replace
		
		
