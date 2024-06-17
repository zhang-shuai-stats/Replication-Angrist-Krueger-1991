* 2024/6/6
clear
cd /Users/zhangshuai/Desktop/interest/angrist1991_replication // 修改默认目录

*-----------------------------------------------------------------------------
* data preparation and label variable 
*-----------------------------------------------------------------------------
use "NEW7080.dta", clear

* 根据angrist网站修改变量名称
rename v1 AGE
rename v2 AGEQ
rename v4 EDUC
rename v5 ENOCENT
rename v6 ESOCENT
rename v9 LWKLYWGE
rename v10 MARRIED
rename v11 MIDATL
rename v12 MT
rename v13 NEWENG
rename v16 CENSUS
rename v17 STATE
rename v18 QOB
rename v19 RACE
rename v20 SMSA
rename v21 SOATL
rename v24 WNOCENT
rename v25 WSOCENT
rename v27 YOB
rename *, lower

* label variables
label variable age "Age: men born between 1920-1929 in 1970; born between 1930-1949 in the 1980"
label variable ageq "Age in quarters"
label variable educ "Years of completed education"
label variable lwklywge "ln (weekly wage)"
label variable race "Race (1=black)"

* 修改sama的值
replace smsa = 1 - smsa
label variable smsa "if works in 标准大都市统计区(Standard Metropolitan Statistic Area) (1 = center city)"

* 修改个别年份，并生成日期
replace yob = yob - 1900 if yob > 1900
replace ageq = ageq - 1900 if ageq > 1900
gen yq = yq(yob+1900,qob)
format yq %tq

label variable yob "Birth year"
label variable qob "Birth quarter"
label variable yq "Birth year quarter"

* 工作周的对数
gen lwk = v8 - lwklywge
label variable lwk "log of weeks worked"

* 生成新的变量，并做ma平减用于表1和图4
gen educ_high = educ if educ > 11 // 高中以上的教育年限
gen high_school = educ > 11  // 是否高中毕业 
gen college = educ > 15  // 是否大学毕业
gen master = educ > 17  // 是否为硕士
gen doctor = educ > 19  // 是否为博士

label variable educ_high "高中以上的教育年限"
label variable high_school "是否高中毕业" 
label variable college  "是否大学毕业"
label variable master  "是否为硕士"
label variable doctor "是否为博士"

tempfile temp
save `temp', replace 

* ma 
local varname educ educ_high high_school college master doctor
collapse (mean) `varname', by(yob qob yq)
tsset yq

foreach i in `varname' {
	tssmooth ma `i'_ma = `i', window(2 0 2)
	drop `i' // 防止融合merge到原始数据中
}

drop if yob < 30 | (yob==30 & qob < 3) | (yob==49 & qob>2) // 相关变量的MA平减仅限于1930-1950，1930年之前的不考虑
merge 1:m yq using `temp', nogen

* detrend 
foreach i in `varname' {
	gen `i'_detrend = `i' - `i'_ma
	label variable `i'_detrend "MA平减后的`i'"
}
drop *_ma

* identifier for residence 八个地区的识别码
egen residence = group(neweng midatl enocent wnocent soatl esocent wsocent mt) 
label variable residence "Identifier for eight region-of-residence"

save angrist1991_data, replace 

* statistics in Angrist's web page
summarize if inrange(yob,30,39)
summarize if inrange(yob,40,49)

*-----------------------------------------------------------------------------
* FIGURE I-III Years of Education and Season of Birth
*-----------------------------------------------------------------------------
use angrist1991_data, clear

count if yob==44 & qob == 1 
collapse (mean) educ, by(yob qob yq)

* figure 1
twoway (connected educ yq if inrange(yob,30,39), msymbol(square) mlabel(qob) mlabposition(6) xtitle("Year of Birth") ytitle("Years of Completed Education")  title("Figure I" "Years of Education and Season of Birth" "1980 Census" "Note: Quarter of birth is listed below each observation", position(6) size(medsmall)) )

graph export "figure1.jpg", as(jpg) quality(100) replace

* figure2
twoway (connected educ yq if inrange(yob,40,49), msymbol(square) mlabel(qob) mlabposition(6) xtitle("Year of Birth") ytitle("Years of Completed Education")  title("Figure II" "Years of Education and Season of Birth" "1980 Census" "Note: Quarter of birth is listed below each observation", position(6) size(medsmall)) )

graph export "figure2.jpg", as(jpg) quality(100) replace

* note: 50-59年数据缺失
*-----------------------------------------------------------------------------
* FIGURE IV Season of Birth and Years of Schooling Deviations from MA (+ 2, —2)
*-----------------------------------------------------------------------------
use angrist1991_data, clear

graph bar (mean) educ_detrend if inrange(yob,30,39), over(qob) over(yob) asyvars bar(1, fcolor(black)) bar(2, fcolor(white)) bar(3, fcolor(white)) bar(4, fcolor(green)) blabel(name) legend(off) name(g1, replace) nodraw ytitle("")

graph bar (mean) educ_detrend if inrange(yob,40,49), over(qob) over(yob) asyvars bar(1, fcolor(black)) bar(2, fcolor(white)) bar(3, fcolor(white)) bar(4, fcolor(green)) blabel(name) legend(off) name(g2, replace) nodraw ytitle("")

graph combine g1 g2, col(1) l1(Schooling Differential) b1(Year of Birth) title("Figure IV" "Season of Birth and Years of Schooling" "Deviations from MA(+2,-2)", position(6) size(medsmall))

graph export "figure4.jpg", as(jpg) quality(100) replace

*-------------------------------------------------------------------------------
* TABLE I THE EFFECT OF QUARTER OF BIRTH ON VARIOUS EDUCATIONAL OUTCOME VARIABLES
*-------------------------------------------------------------------------------
use angrist1991_data, clear

drop if (yob==30 & qob < 3) | (yob==49 & qob>2)
gen cohort = .
replace cohort = 1 if inrange(yob,30,39)
replace cohort = 2 if inrange(yob,40,49)

capture program drop create_table1
program create_table1 
	args var ind 
	qui: collect _r_b _r_se, tags(coleq[`var'_detrend] cmdset[`ind']):reg `var'_detrend ib4.qob if cohort == `ind'
	scalar F = e(F)
	scalar Fp = Ftail(e(df_m), e(df_r), e(F))	
	collect get _r_b = F _r_se = Fp, tags(coleq[`var'_detrend] cmdset[`ind'] colname[F])
	
	qui: summarize `var' if cohort == `ind'
	collect get _r_b = r(mean), tags(coleq[`var'_detrend] cmdset[`ind'] colname[mean])
end

collect clear
local varname educ high_school educ_high college master doctor
foreach var in `varname' {
	create_table1 `var' 1 
	create_table1 `var' 2 
}

* format 
collect dims
collect levelsof colname
collect levelsof result
collect levelsof qob
collect levelsof coleq
collect layout (coleq#cmdset#result) (colname[mean] qob[1 2 3] colname[F])

* row format 
collect style header result, level(hide)
collect label levels cmdset 1 "1930-1939" 2 "1940-1949", modify

local varname educ high_school educ_high college master doctor
local colname `" "Total years of education" "High school graduate" "Years of educ. for high school graduates" "College graduate" "Completed master degree" "Completed doctoral degree" "'
local var_count  `:word count `varname''
forvalues i = 1/`var_count' {
	local tmp `:word `i' of `varname''
	local tmp1 `:word `i' of `colname''
	collect label levels coleq `"`tmp'_detrend"' `"`tmp1'"', modify
}

* column format
collect style header qob, title(label)
collect style column, nodelimiter dups(center) position(top) width(asis)
collect label dim qob "Quarter-of-birth effect", modify
collect label levels colname mean "Mean" F "F-test [P-Value]", modify

* cell format
collect style cell colname[mean], warn halign(center) valign(center) nformat(%9.2f)
collect style cell qob, warn halign(center) valign(center) nformat(%9.3f)
collect style cell qob#result[_r_se], sformat("(%s)") warn
collect style cell colname[F], warn halign(center) valign(center) nformat(%9.1f)
collect style cell colname[F]#result[_r_se], nformat(%9.4f) sformat("[%s]")

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "TABLE I - THE EFFECT OF QUARTER OF BIRTH ON VARIOUS EDUCATIONAL OUTCOME VARIABLES"

collect layout (coleq#cmdset#result) (colname[mean] qob[1 2 3] colname[F])
collect export "table1", as(docx) replace	

*-----------------------------------------------------------------------------
* FIGURE V: Mean Log Weekly Wage, by Quarter of Birth All Men Born 1930-1949; 1980 Census
*-----------------------------------------------------------------------------
use angrist1991_data, clear

collapse (mean) lwklywge, by(yob qob yq)

* figure 5
twoway (connected lwklywge yq if yob > 29, mcolor(white) msymbol(square) mlabel(qob) mlcolor(black)) (scatter lwklywge yq if qob == 1 & yob > 29, mcolor(black) msymbol(square)), xtitle("Year of Birth") ytitle("Log Weekly Earnings")  title("Figure V" "Mean Log Weekly Wage, by Quarter of Birth" " All Men Born 1930-1949; 1980 Census", position(6) size(medsmall)) legend(off)

graph export "figure5.jpg", as(jpg) quality(100) replace

*-----------------------------------------------------------------------------
* TABLE III 
*-----------------------------------------------------------------------------
use angrist1991_data, clear

gen q_dum = qob == 1
capture program drop create_table3
program create_table3 
	args part row
	if inlist(`row',1,2) {
		scalar coef1 = _b[q_dum] + _b[_cons]
		scalar coef2 = _b[_cons]
		scalar coef3 = _b[q_dum]
		scalar se3 = _se[q_dum]
		collect get coef = coef1, tags(part[`part'] row[`row'] col[1])
		collect get coef = coef2, tags(part[`part'] row[`row'] col[2])
		collect get coef = coef3, tags(part[`part'] row[`row'] col[3])
		collect get se = se3, tags(part[`part'] row[`row'] col[3])
	}
	else {
		scalar coef3 = _b[educ]
		scalar se3 = _se[educ]
		collect get coef = coef3, tags(part[`part'] row[`row'] col[3])
		collect get se = se3, tags(part[`part'] row[`row'] col[3])		
	}
end

* PANEL A: WALD ESTIMATES FOR 1970 CENSUS-MEN BORN 1920-1929
collect clear
qui: reg lwklywge q_dum if yob < 30
create_table3 a 1
qui: reg educ q_dum if yob < 30
create_table3 a 2
qui: ivregress 2sls lwklywge (educ=q_dum) if yob < 30
create_table3 a 3
qui: reg lwklywge educ if yob < 30 
create_table3 a 4

* Panel B: Wald Estimates for 1980 Census—Men Born 1930-1939
qui: reg lwklywge q_dum if inrange(yob,30,39) 
create_table3 b 1
qui: reg educ q_dum if inrange(yob,30,39) 
create_table3 b 2
qui: ivregress 2sls lwklywge (educ=q_dum) if inrange(yob,30,39) 
create_table3 b 3
qui: reg lwklywge educ if inrange(yob,30,39) 
create_table3 b 4

* format 
collect dims
collect layout (part#row#result) (col)

* row format 
collect style header result, level(hide)
collect label levels part a "PANEL A: WALD ESTIMATES FOR 1970 CENSUS-MEN BORN 1920-1929" b "Panel B: Wald Estimates for 1980 Census—Men Born 1930-1939", modify
local colname `" "ln (wkly. wage)" "Education" "Wald est. of return to education" "OLS return to education" "'
forvalues i = 1/4 {
	local tmp `:word `i' of `colname''
	collect label levels row `"`i'"' `"`tmp'"', modify
}

* column format
local colname `" "Born in 1st quarter of year" "Born in 2nd, 3rd, or 4th quarter of year" "Difference (std. error) (1)-(2)" "'
forvalues i = 1/3 {
	local tmp `:word `i' of `colname''
	collect label levels col `"`i'"' `"`tmp'"', modify
}

* cell format
collect style cell col, warn halign(center) valign(center) nformat(%9.4f)
collect style cell row[1]#col[3], nformat(%9.5f)
collect style cell result[se], sformat("(%s)") warn

* table format
collect style cell border_block, border(right, pattern(nil))
collect title "TABLE III - WALD Estimate"

collect layout (part#row#result) (col)
collect export "table3", as(docx) replace	

*-----------------------------------------------------------------------------
* TABLE IV--VIII
*-----------------------------------------------------------------------------
* 由于表4之后的各个表格格式一致，可以设置函数输出最终结果
capture program drop collect_save  
program collect_save // 收集除了reg命令之外的其他结果
	args cmdset dum1 dum2 table_id
	if mod(`cmdset',2) == 1 {
		collect get year_dummy = `"`dum1'"', tags(cmdset[`cmdset'] colname[year_dummy])
		collect get region_dummy = `"`dum2'"', tags(cmdset[`cmdset'] colname[region_dummy])		
	}
	else {
		estat endogenous  // 内生性检验（文中表格未列示）
		scalar wu = r(wu)
		scalar p_wu = r(p_wu)
		estat overid  // 过度识别检验
		scalar dof = r(df)
		scalar chi2 = r(sargan)

		collect get wu = wu, tags(cmdset[`cmdset'])
		collect get p_wu = p_wu, tags(cmdset[`cmdset'])
		collect get chi = chi2, tags(cmdset[`cmdset'])
		collect get dof = dof, tags(cmdset[`cmdset'])
		collect get year_dummy = `"`dum1'"', tags(cmdset[`cmdset'] colname[year_dummy])
		collect get region_dummy = `"`dum2'"', tags(cmdset[`cmdset'] colname[region_dummy])
	}
	if `table_id' > 6 {
		collect get state_dummy = "Yes", tags(cmdset[`cmdset'] colname[state_dummy])  // 为表7和表8添加state dummies
	}
	
end

capture program drop create_table4
program create_table4
	args table_id
	qui: {
		collect clear
		if inlist(`table_id',4,5,6) { // 表4-表6
			* column 1
			collect: reg lwklywge educ i.yob   
			collect_save 1 Yes No `table_id'
			
			* column 2
			collect: ivregress 2sls lwklywge i.yob (educ = qob#yob) 
			collect_save 2 Yes No `table_id'

			* column 3
			collect: reg lwklywge educ i.yob ageq c.ageq#c.ageq 
			collect_save 3 Yes No `table_id'

			* column 4
			collect: ivregress 2sls lwklywge i.yob ageq c.ageq#c.ageq (educ = qob#yob) 
			collect_save 4 Yes No `table_id'

			* column 5
			collect: reg lwklywge educ race smsa married i.yob i.residence 
			collect_save 5 Yes Yes `table_id'

			* column 6
			collect: ivregress 2sls lwklywge race smsa married i.yob i.residence (educ = qob#yob)
			collect_save 6 Yes Yes `table_id'

			* column 7
			collect: reg lwklywge educ race smsa married i.yob i.residence ageq c.ageq#c.ageq 
			collect_save 7 Yes Yes `table_id'

			* column 8
			collect: ivregress 2sls lwklywge race smsa married i.yob i.residence ageq c.ageq#c.ageq (educ = qob#yob)
			collect_save 8 Yes Yes `table_id'
		}
		else {  // 表7-表8		
			* column 1
			collect: reg lwklywge educ i.yob i.state 
			collect_save 1 Yes No `table_id'

			* column 2
			collect: ivregress 2sls lwklywge i.yob i.state (educ = qob#yob qob#state) 				
			collect_save 2 Yes No `table_id'

			* column 3
			collect: reg lwklywge educ i.yob ageq c.ageq#c.ageq i.state 
			collect_save 3 Yes No `table_id'

			* column 4
			collect: ivregress 2sls lwklywge i.yob ageq c.ageq#c.ageq i.state (educ = qob#yob qob#state) 				
			collect_save 4 Yes No `table_id'

			* column 5
			collect: reg lwklywge educ race smsa married i.yob i.residence i.state 			
			collect_save 5 Yes Yes `table_id'

			* column 6
			collect: ivregress 2sls lwklywge race smsa married i.yob i.residence i.state (educ = qob#yob qob#state) 
			collect_save 6 Yes Yes `table_id'

			* column 7
			collect: reg lwklywge educ race smsa married i.yob i.residence ageq c.ageq#c.ageq i.state 				
			collect_save 7 Yes Yes `table_id'

			* column 8
			collect: ivregress 2sls lwklywge race smsa married i.yob i.residence ageq c.ageq#c.ageq i.state (educ = qob#yob qob#state) 
			collect_save 8 Yes Yes `table_id'
		}
		* format 
		collect dims
		collect levelsof colname
		collect levelsof result
		collect levelsof yob
		collect levelsof coleq
		collect layout (colname[educ race smsa married year_dummy region_dummy ageq ageq#ageq]#result[_r_b _r_se year_dummy region_dummy] result[chi dof wu p_wu]) (cmdset)

		* row format 
		collect composite define chi2d = chi dof, trim replace
		collect composite define wup = wu p_wu, trim replace
		collect label levels result chi2d "Chi2[dof]", modify
		collect label levels result wup "Wu-test[p]", modify
		collect style header result, level(hide)
		collect style header result[chi2d wup], level(label)
		
		local varname educ race smsa married year_dummy region_dummy ageq ageq#ageq
		local colname `" "Years of education" "Race (1 = black)" "SMSA (1 = center city)" "Married (1 = married)" "9 Year-of-birth dummies" "8 Region of residence dummies" "Age" "Age-squared" "'
		local var_count  `:word count `varname''
		forvalues i = 1/`var_count' {
			local tmp `:word `i' of `varname''
			local tmp1 `:word `i' of `colname''
			collect label levels colname `"`tmp'"' `"`tmp1'"', modify
		}
		collect label levels colname state_dummy "50 State-of-birth dummies", modify
		
		* cell format
		collect style cell cmdset, warn halign(center) valign(center) 
		collect style cell colname[`varname'], warn nformat(%9.4f)
		collect style cell result[_r_se], sformat("(%s)") warn
		collect style cell result[chi wu],nformat(%9.1f) warn
		collect style cell result[dof], nformat(%9.0f) sformat("[%s]") warn
		collect style cell result[p_wu],nformat(%9.2f) sformat("[%s]") warn

		* table format
		collect style cell border_block, border(right, pattern(nil))
	}
		collect layout (colname[educ race smsa married year_dummy region_dummy state_dummy ageq ageq#ageq]#result[_r_b _r_se year_dummy region_dummy state_dummy] result[chi2d wup]) (cmdset)

end

use angrist1991_data, clear

* TABLE IV: OLS AND TSLS ESTIMATES OF THE RETURN TO EDUCATION FOR MEN BORN 1920-1929: 1970 CENSUS
use angrist1991_data, clear 
keep if yob < 30
create_table4 4
collect title "TABLE IV--OLS AND TSLS ESTIMATES OF THE RETURN TO EDUCATION FOR MEN BORN 1920-1929: 1970 CENSUS"
collect export "table4", as(docx) replace

* TABLE V: OLS AND TSLS ESTIMATES OF THE RETURN TO EDUCATION FOR MEN BORN 1930-1939: 1980 CENSUS
use angrist1991_data, clear 
keep if inrange(yob,30,39)
create_table4 5
collect title "TABLE V--OLS AND TSLS ESTIMATES OF THE RETURN TO EDUCATION FOR MEN BORN 1930-1939: 1980 CENSUS"
collect export "table5", as(docx) replace	


* TABLE VI: OLS AND TSLS ESTIMATES OF THE RETURN TO EDUCATION FOR MEN BORN 1940-1949: 1980 CENSUS
use angrist1991_data, clear 
keep if inrange(yob,40,49)
create_table4 6
collect title "TABLE VI--OLS AND TSLS ESTIMATES OF THE RETURN TO EDUCATION FOR MEN BORN 1940-1949: 1980 CENSUS"
collect export "table6", as(docx) replace	

* statistics in page 1002: slightly negative and insignificant Wald estimate
gen q_dum = qob == 1
ivregress 2sls lwklywge (educ=q_dum) if inrange(yob,40,49)

* statistics in p1002: when the log of weeks worked is used as dependent variable
use angrist1991_data, clear 
keep if inrange(yob,30,39)
reg lwk educ race smsa married i.yob i.residence if inrange(yob,30,39)
ivregress 2sls lwk race smsa married i.yob i.residence (educ = qob#yob )

* TABLE VII: OLS AND TSLS ESTIMATES OF THE RETURN TO EDUCATION FOR MEN BORN 1930-1939: 1980 CENSUS
use angrist1991_data, clear
keep if inrange(yob,30,39)
create_table4 7
collect title "TABLE VII--OLS AND TSLS ESTIMATES OF THE RETURN TO EDUCATION FOR MEN BORN 1930-1939: 1980 CENSUS"
collect export "table7", as(docx) replace	

* statistics in p1005: for men born 1930-1939 with nine to twelve years of education
keep if inrange(yob,30,39) & inrange(educ,9,12)
reg lwklywge educ race smsa married i.yob i.residence 
ivregress 2sls lwklywge race smsa married i.yob i.residence (educ = qob#yob )
* mote: OLS can produce similar results, but not TSLS

* TABLE VIII: OLS AND TSLS ESTIMATES OF THE RETURN TO EDUCATION FOR BLACK MEN BORN 1930-1939: 1980 CENSUS
use angrist1991_data, clear
keep if inrange(yob,30,39) & race == 1
create_table4 8
collect title "TABLE VIII--OLS AND TSLS ESTIMATES OF THE RETURN TO EDUCATION FOR BLACK MEN BORN 1930-1939: 1980 CENSUS"
collect export "table8", as(docx) replace	

* statistics in p1008: adding three quarter-of-birth dummies to the OLS models for the sample of prime-age men in column 5 of Tables IV and V.
use angrist1991_data, clear

qui: reg lwklywge educ race smsa married i.yob i.residence i.qob if yob < 30 
testparm i(2/4).qob

qui: reg lwklywge educ race smsa married i.yob i.residence i.qob if inrange(yob,30,39) 
testparm i(2/4).qob

* statistics in p1009: using the sample of 40-49-year old college graduates in the 1970 Census
qui: reg lwklywge  i.yob  i.qob if yob < 30 & college == 1 
testparm i(2/4).qob

qui: reg lwklywge  i.yob  i.qob if inrange(yob,30,39) & college == 1 
testparm i(2/4).qob


	
