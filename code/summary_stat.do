clear all
local dataraw "C:\Users\riana\OneDrive - UC San Diego\2025 Fall\Labor\VSP\data_vsp\raw"
local output "C:\Users\riana\OneDrive - UC San Diego\2025 Fall\Labor\VSP\output"
local inter    "C:\\Users\\riana\\OneDrive - UC San Diego\\2025 Fall\\Labor\\VSP\\data_vsp\\intermediate"
local output "C:\\Users\\riana\\OneDrive - UC San Diego\\2025 Fall\\Labor\\VSP\\output"




/*use "`dataraw'/level_1a.dta", replace
* identifiant: project_location_id 

destring transactions_start_year transactions_end_year, replace
*/


*-------------------------------------------------------------*
* Summary Statistics by Year (means & SDs in parentheses)
*-------------------------------------------------------------*

* List of variables
use "`inter'/education", replace
encode region, gen(region_id)
xtset region_id year


cap program drop meansd
program define meansd 
		sum `1' if year==`2'
		local mean = strofreal(`r(mean)',"%9.2f")
		local se = strofreal(`r(sd)',"%9.2f")
		local obs = strofreal(`r(N)',"%9.2f")
		putexcel `3'1=`2', hcenter 
		putexcel `3'`5'="`mean'", hcenter border(top) 
		putexcel `3'`6'="(`se')", hcenter 
		putexcel B`4'=`obs', hcenter border(top)
end

putexcel set "`output'\\summary_table.xlsx", replace
*use "`inter'/education", replace
	*foreach num of numlist 2014/2017 {}
meansd prim_ger 2014 C 3 3 4
meansd prim_ger 2015 D 3 3 4
meansd prim_ger 2016 E 3 3 4
meansd prim_ger 2017 F 3 3 4


meansd sec_ger_ 2014 C 5 5 6
meansd sec_ger_ 2015 D 5 5 6
meansd sec_ger_ 2016 E 5 5 6
meansd sec_ger_ 2017 F 5 5 6

mata
b = xl()
b.load_book("summary_table.xlsx")
b.set_sheet("summ")
b.set_column_width(1,1,20) // make variable name column widest
b.set_column_width(2,3,16) // width for subsequent columns
b.set_row_height(7,7,32)
b.close_book()
end



