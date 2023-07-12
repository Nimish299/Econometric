** Use "A. Raw DTA File - GLUED_Institutional_orig.dta"

encode region, generate (regionnum)

encode incomegroup, generate (incomegroupnum)

encode iau_id , generate (iau_id_num)

preserve

keep iau_id_num year students5_estimated regionnum incomegroupnum foundedyr private01 phd_granting m_granting b_granting divisions total_fields unique_fields specialized  

drop if m_granting == .
drop if students5_estimated == .
drop if regionnum == .

reshape wide students5_estimated regionnum incomegroupnum foundedyr private01 phd_granting m_granting b_granting divisions total_fields unique_fields specialized, i(iau_id_num) j(year)
drop if foundedyr2010 == . | foundedyr2015 == . | foundedyr2020 == .
reshape long students5_estimated regionnum incomegroupnum foundedyr private01 phd_granting m_granting b_granting divisions total_fields unique_fields specialized, i(iau_id_num) j(year)

drop if year == 2000
drop if year == 2005

xtset iau_id_num year

** Hence, we see that the resulting dataset is Strongly Balance Panel Dataset. We save this dataset in "B. Final Balanced Panel Dataset.dta".
