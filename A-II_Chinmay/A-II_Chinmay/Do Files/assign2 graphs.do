graph pie, over(specialized) pie(1, color(pink)) pie(2, color(midblue)) plabel(_all percent) legend(order(1 "Non-Specialized" 2 "Specialized"))

graph pie, over(private01) pie(1, color(pink)) pie(2, color(midblue)) plabel(_all percent) legend(order(1 "Public" 2 "Private"))

tab regionnum

graph bar (mean) students5_estimated, over(regionnum, relabel(1 "East Asia and Pacific" 2 "Europe and Central Asia" 3 " Latin America and Caribbean" 4 "Middle East and North Africa" 5 "North America" 6 " South Asia" 7 "Sub-Saharan Africa")) over(year) bar(2, fcolor("128 255 128")) blabel(bar, size(small)) ytitle(Number of Students Enrolled) legend(size(vsmall))


histogram phd_granting, discrete percent addlabel xlabel(0(1)1) legend(order(1 "No Grant" 2 "Grant"))(start=0, width=1)

histogram m_granting, discrete percent addlabel xlabel(0(1)1) legend(order(1 "No Grant" 2 "Grant"))(start=0, width=1)

histogram b_granting, discrete percent addlabel xlabel(0(1)1) legend(order(1 "No Grant" 2 "Grant"))(start=0, width=1)

graph bar (percent), over(specialized, relabel(1 "Non-Specialized" 2 "Specialized")) bar(2, fcolor("128 255 128")) blabel(bar, size(small)) ytitle(Number of Students Enrolled) legend(size(vsmall))

