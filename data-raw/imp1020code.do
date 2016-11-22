**get data & reshape long
insheet using "C:\Users\stepharp\Desktop\FCTC\Implemenation data 10.20.14\Main.csv", names
reshape long var1 var2 var3 var4 var5   var6   var7   var8   var9   var10  var11  var12 var13  var14  var15  var16  var17  var18  var19  var20  var21  var22  var23  var24 var25  var26  var27  var28  var29  var30  var31  var32  var33  var34  var35  var36 var37  var38  var39  var40  var41  var42  var43  var44  var45  var46  var47  var48 var49  var50  var51  var52  var53  var54  var55  var56  var57  var58  var59  var60  var61  var62  var63  var64  var65  var66  var67  var68  var69  var70  var71  var72  var73  var74  var75  var76  var77  var78  var79  var80  var81  var82  var83  var84  var85  var86  var87  var88  var89  var90  var91  var92  var93  var94  var95  var96   var97  var98  var99  var100 var101 var102 var103 var104 var105 var106 var107 var108 var109 var110 var111 var112 var113 var114 var115 var116 var117 var118 var119 var120 var121 var122 var123 var124 var125 var126 var127 var128 var129 var130 var131 var132 var133 var134 var135 var136 var137 var138 var139 var140 var141 var142 var143 var144 var145 var146 var147 var148 var149 var150 var151 var152 var153 var154 var155 var156 var157 var158 var159 var160 var161 var162 var163 var164 var165 var166 var167 var168 var169 var170 var171 var172 var173 var174 var175 var176 var177 var178 var179 var180 var181 var182 var183 var184 var185 var186 var187 var188 var189 var190 var191 var192 var193 var194 var195 var196 var197 var198 var199 var200 var201 var202 var203 var204 var205 var206 var207 var208 var209 var210 var211 var212 var213 var214 var215 var216 var217 var218 var219 var220 var221 var222 var223 var224 var225 var226 var227 var228 var229 var230 var231 var232 var233 var234 var235 var236 var237 var238 var239 var240 var241 var242 var243 var244 var245 var246 var247 var248 var249 var250 var251 var252 var253 var254 var255 var256 var257 var258 var259 var260 var261 var262 var263 var264 var265 var266 var267 var268 var269 var270 var271 var272 var273 var274 var275 var276 var277 var278 var279 var280 var281 var282 var283 var284 var285 var286 var287 var288 var289 var290 var291 var292 var293 var294 var295 var296 var297 var298, i(country) j("x" "x1" "x2" "x3") string
save "C:\Users\stepharp\Desktop\FCTC\Implemenation data 10.20.14\rawimp.dta"
***rename to numerics and destring
replace x="2014" if x=="x3"
replace x="2012" if x=="x2"
replace x="2010" if x=="x1"
save "C:\Users\stepharp\Desktop\FCTC\Implemenation data 10.20.14\rawimp.dta", replace
foreach var of varlist var1-var298 {
replace `var'="." if `var'=="Answer/report not provided"
replace `var'="." if `var'=="Indicator not used"
replace `var'="1" if `var'=="Yes"
replace `var'="0" if `var'=="No"
replace `var'="0" if `var'=="None"
replace `var'="" if `var'=="Not applicable"
replace `var'="1" if `var'=="Full"
replace `var'=".5" if `var'=="Partially"
replace `var'=".5" if `var'=="Partial"
}
destring, replace
save "C:\Users\stepharp\Desktop\FCTC\Implemenation data 10.20.14\numericimp.dta"
****** string var1 var2 var7 var8 var21 var23 var24 var46 var56 var57 var81 var85 var117 var118 var121 var133 var134 var135 var146 var147 var150 var158 var165 var166 var167 var168 var173 var178 var183 var189 var190 var196 var206 var207 var210 var211 var215 var216 var220 var221 var222 var224 var229 var230 var235 var236 var242 var248 var249 var252 var253 var254 var257 var260 var268 var269 var270 var275 var288 var289 var291 var292 var293 var294 var295 var296 var298
summ var1 var2 var7 var8 var21 var23 var24 var46 var56 var57 var81 var85 var117 var118 var121 var133 var134 var135 var146 var147 var150 var158 var165 var166 var167 var168 var173 var178 var183 var189 var190 var196 var206 var207 var210 var211 var215 var216 var220 var221 var222 var224 var229 var230 var235 var236 var242 var248 var249 var252 var253 var254 var257 var260 var268 var269 var270 var275 var288 var289 var291 var292 var293 var294 var295 var296 var298
drop var1 var2 var7 var8 var21 var23 var24 var46 var56 var57 var81 var85 var117 var118 var121 var133 var134 var135 var146 var147 var150 var158 var165 var166 var167 var168 var173 var178 var183 var189 var190 var196 var206 var207 var210 var211 var215 var216 var220 var221 var222 var224 var229 var230 var235 var236 var242 var248 var249 var252 var253 var254 var257 var260 var268 var269 var270 var275 var288 var289 var291 var292 var293 var294 var295 var296 var298
egen meanall= rmean( var3-var297)
summ meanall
gen data=""
replace data="No data" if meanall==.
ren x year
bysort year: tab data
*** year 2012 has most complete data, 2010 the least

***not all questions are asked at all years. Let's go ahead and drop variables not included in 2012
drop var182 var181 var180 var179 var11 var9
summ var3-var297 if year==2012
egen art10miss = rowmiss( var3 var4 var5 var6)
egen art11miss = rowmiss( var10  var12  var13  var14  var15  var16  var17  var18  var19  var20  var22 )
egen art12miss = rowmiss(var25  var26  var27  var28  var29  var30  var31  var32  var33  var34  var35  var36  var37  var38  var39  var40  var41  var42  var43  var44  var45  var47  var48  var49  var50  var51  var52  var53  var54  var55 )
egen art13miss = rowmiss( var58  var59  var60  var61  var62  var63  var64  var65  var66  var67  var68  var69  var70  var71  var72  var73  var74  var75  var76  var77  var78  var79  var80  var82  var83  var84  var86 )
egen art14miss = rowmiss( var87  var88  var89  var90  var91  var92  var93  var94  var95  var96  var97  var98  var99  var100  var101  var102  var103  var104  var105  var106  var107  var108  var109  var110  var111  var112  var113  var114  var115  var116  var119  var120  var122  var123  var124  var125  var126  var127  var128  var129  var130  var131  var132 )
egen art15miss = rowmiss(  var136  var137  var138  var139  var140  var141  var142  var143  var144  var145  var148  var149)
egen art16miss = rowmiss( var151  var152  var153  var154  var155  var156  var157  var159  var160  var161  var162  var163  var164 )
egen art17miss = rowmiss( var169 var170 var171 var172 )
egen art18miss = rowmiss( var174 var175 var176 var177 )
egen art20miss = rowmiss( var184 var185 var186 var187 var188 var191 var192 var193 var194 var195 var197 var198 var199 var200 var201 var202 var203 var204 var205 )
egen art6miss = rowmiss( var223  var225  var226  var227  var228  var231  var232  var233  var234 )
egen art8miss = rowmiss( var237  var238  var239  var240  var241  var243  var244  var245  var246  var247  var250  var251  var255  var256  var258  var259  var261  var262  var263  var264  var265  var266  var267 )
egen art9miss = rowmiss( var271  var272  var273  var274 )
egen art2226assistprovidedmiss = rowmiss(var276 var277 var278 var279 var280 var281)
egen art2226assistreceivedmiss = rowmiss( var282  var283  var284  var285  var286  var287 )
egen art2226otherassistancemiss = rowmiss( var290 )
egen art2226prioritiesmiss = rowmiss( var297 )
gen art10missp = art10miss/4
gen art11missp = art11miss/11
gen art12missp = art12miss/30
gen art13missp = art13miss/27
gen art14missp = art14miss/43
gen art15missp = art15miss/12
gen art16missp = art16miss/13
gen art17missp = art17miss/4
gen art18missp = art18miss/4
gen art20missp = art20miss/19
gen art6missp = art6miss/9
gen art8missp = art8miss/23
gen art9missp = art9miss/4
gen art2226assistprovidedmissp =  art2226assistprovidedmiss/6
gen art2226assistreceivedmissp = art2226assistreceivedmiss/6
gen art2226otherassistancemissp = art2226otherassistancemiss
gen art2226prioritiesmissp = art2226prioritiesmiss

summ art10missp art11missp art12missp art13missp art14missp art15missp art16missp art17missp art18missp art20missp art6missp art8missp art9missp art2226assistprovidedmissp art2226assistreceivedmissp art2226otherassistancemissp art2226prioritiesmissp
summ art10missp art11missp art12missp art13missp art14missp art15missp art16missp art17missp art18missp art20missp art6missp art8missp art9missp art2226assistprovidedmissp art2226assistreceivedmissp art2226otherassistancemissp art2226prioritiesmissp if year==2012
egen art10 = rmean( var3 var4 var5 var6)
egen art11 = rmean( var10  var12  var13  var14  var15  var16  var17  var18  var19  var20  var22 )
egen art12 = rmean(var25  var26  var27  var28  var29  var30  var31  var32  var33  var34  var35  var36  var37  var38  var39  var40  var41  var42  var43  var44  var45  var47  var48  var49  var50  var51  var52  var53  var54  var55 )
egen art13 = rmean( var58  var59  var60  var61  var62  var63  var64  var65  var66  var67  var68  var69  var70  var71  var72  var73  var74  var75  var76  var77  var78  var79  var80  var82  var83  var84  var86 )
egen art14 = rmean( var87  var88  var89  var90  var91  var92  var93  var94  var95  var96  var97  var98  var99  var100  var101  var102  var103  var104  var105  var106  var107  var108  var109  var110  var111  var112  var113  var114  var115  var116  var119  var120  var122  var123  var124  var125  var126  var127  var128  var129  var130  var131  var132 )
egen art15 = rmean(  var136  var137  var138  var139  var140  var141  var142  var143  var144  var145  var148  var149)
egen art16 = rmean( var151  var152  var153  var154  var155  var156  var157  var159  var160  var161  var162  var163  var164 )
egen art17 = rmean( var169 var170 var171 var172 )
egen art18 = rmean( var174 var175 var176 var177 )
egen art20 = rmean( var184 var185 var186 var187 var188 var191 var192 var193 var194 var195 var197 var198 var199 var200 var201 var202 var203 var204 var205 )
egen art6 = rmean( var223  var225  var226  var227  var228  var231  var232  var233  var234 )
egen art8 = rmean( var237  var238  var239  var240  var241  var243  var244  var245  var246  var247  var250  var251  var255  var256  var258  var259  var261  var262  var263  var264  var265  var266  var267 )
egen art9 = rmean( var271  var272  var273  var274 )
egen art2226assistprovided = rmean(var276 var277 var278 var279 var280 var281)
egen art2226assistreceived = rmean( var282  var283  var284  var285  var286  var287 )
egen art2226otherassistance= rmean( var290 )
egen art2226priorities = rmean( var297 )
sum art10 art11 art12 art13 art14 art15 art16 art17 art18 art20 art6 art8 art9 art2226assistprovided art2226assistreceived art2226otherassistance art2226priorities meanallart
***item 156 is min age to purchase tobacco products, don't want to include this actually redo art16 and mean
drop art16
egen art16 = rmean( var151  var152  var153  var154  var155   var157  var159  var160  var161  var162  var163  var164 )

egen meanall= rmean( var3  var4  var5  var6  var10  var12  var13  var14  var15  var16  var17  var18  var19  var20  var22  var25  var26  var27  var28  var29  var30  var31  var32  var33  var34  var35  var36  var37  var38  var39  var40  var41  var42  var43  var44  var45  var47  var48  var49  var50  var51  var52  var53  var54  var55  var58  var59  var60  var61  var62  var63  var64  var65  var66  var67  var68  var69  var70  var71  var72  var73  var74  var75  var76  var77  var78  var79  var80  var82  var83  var84  var86  var87  var88  var89  var90  var91  var92  var93  var94  var95  var96  var97  var98  var99  var100  var101  var102  var103  var104  var105  var106  var107  var108  var109  var110  var111  var112  var113  var114  var115  var116  var119  var120  var122  var123  var124  var125  var126  var127  var128  var129  var130  var131  var132  var136  var137  var138  var139  var140  var141  var142  var143  var144  var145  var148  var149  var151  var152  var153  var154  var155    var157  var159  var160  var161  var162  var163  var164  var169  var170  var171  var172  var174  var175  var176  var177  var184  var185  var186  var187  var188  var191  var192  var193  var194  var195  var197  var198  var199  var200  var201  var202  var203  var204  var205  var208  var209  var212  var213  var214  var217  var218  var219  var223  var225  var226  var227  var228  var231  var232  var233  var234  var237  var238  var239  var240  var241  var243  var244  var245  var246  var247  var250  var251  var255  var256  var258  var259  var261  var262  var263  var264  var265  var266  var267  var271  var272  var273  var274  var276  var277  var278  var279  var280  var281  var282  var283  var284  var285  var286  var287  var290  var297 )
egen meanallart = rmean( art10 art11 art12 art13 art14 art15 art16 art17 art18 art20 art6 art8 art9 art2226assistprovided art2226assistreceived art2226otherassistance art2226priorities)
sum art10 art11 art12 art13 art14 art15 art16 art17 art18 art20 art6 art8 art9 art2226assistprovided art2226assistreceived art2226otherassistance art2226priorities meanallart meanall
sum art10 art11 art12 art13 art14 art15 art16 art17 art18 art20 art6 art8 art9 art2226assistprovided art2226assistreceived art2226otherassistance art2226priorities meanallart meanall if year==2012


egen art1050 = rmean( var3 var4 var5 var6) if art10missp <=.5
egen art1150 = rmean( var10  var12  var13  var14  var15  var16  var17  var18  var19  var20  var22 ) if art11missp <=.5
egen art1250 = rmean(var25  var26  var27  var28  var29  var30  var31  var32  var33  var34  var35  var36  var37  var38  var39  var40  var41  var42  var43  var44  var45  var47  var48  var49  var50  var51  var52  var53  var54  var55 ) if art12missp <=.5
egen art1350 = rmean( var58  var59  var60  var61  var62  var63  var64  var65  var66  var67  var68  var69  var70  var71  var72  var73  var74  var75  var76  var77  var78  var79  var80  var82  var83  var84  var86 ) if art13missp <=.5
egen art1450 = rmean( var87  var88  var89  var90  var91  var92  var93  var94  var95  var96  var97  var98  var99  var100  var101  var102  var103  var104  var105  var106  var107  var108  var109  var110  var111  var112  var113  var114  var115  var116  var119  var120  var122  var123  var124  var125  var126  var127  var128  var129  var130  var131  var132 ) if art14missp <=.5
egen art1550 = rmean(  var136  var137  var138  var139  var140  var141  var142  var143  var144  var145  var148  var149) if art15missp <=.5
egen art1650 = rmean( var151  var152  var153  var154  var155   var157  var159  var160  var161  var162  var163  var164 ) if art16missp <=.5
egen art1750 = rmean( var169 var170 var171 var172 ) if art17missp <=.5
egen art1850 = rmean( var174 var175 var176 var177 ) if art18missp <=.5
egen art2050 = rmean( var184 var185 var186 var187 var188 var191 var192 var193 var194 var195 var197 var198 var199 var200 var201 var202 var203 var204 var205 ) if art20missp <=.5
egen art650 = rmean( var223  var225  var226  var227  var228  var231  var232  var233  var234 ) if art6missp <=.5
egen art850 = rmean( var237  var238  var239  var240  var241  var243  var244  var245  var246  var247  var250  var251  var255  var256  var258  var259  var261  var262  var263  var264  var265  var266  var267 ) if art8missp <=.5
egen art950 = rmean( var271  var272  var273  var274 ) if art9missp <=.5
egen art2226assistprovided50 = rmean(var276 var277 var278 var279 var280 var281) if art2226assistprovidedmissp <=.5
egen art2226assistreceived50 = rmean( var282  var283  var284  var285  var286  var287 ) if art2226assistreceivedmissp <=.5
egen art2226otherassistance50= rmean( var290 ) if art2226otherassistancemissp <=.5
egen art2226priorities50 = rmean( var297 ) if art2226prioritiesmissp <=.5
egen meanallart50 = rmean( art1050 art1150 art1250 art1350 art1450 art1550 art1650 art1750 art1850 art2050 art650 art850 art950 art2226assistprovided50 art2226assistreceived50 art2226otherassistance50 art2226priorities50) 

summ art1050 art1150 art1250 art1350 art1450 art1550 art1650 art1750 art1850 art2050 art650 art850 art950 art2226assistprovided50 art2226assistreceived50 art2226otherassistance50 art2226priorities50 meanallart50
summ art1050 art1150 art1250 art1350 art1450 art1550 art1650 art1750 art1850 art2050 art650 art850 art950 art2226assistprovided50 art2226assistreceived50 art2226otherassistance50 art2226priorities50 meanallart50 if year==2012
sort year meanallart50
order year country meanallart50

save "C:\Users\stepharp\Desktop\FCTC\Implemenation data 10.20.14\impprocessed.dta"


***********************edit 04/14/2015

use "C:\Users\stepharp\Desktop\FCTC\Implemenation data 10.20.14\impprocessed.dta", replace
ren country countrynameun
replace countrynameun="Venezuela (Bolivarian Republic of)" if countrynameun=="Venezuela"
replace countrynameun="United Kingdom of Great Britain and Northern Ireland" if countrynameun=="United Kingdom"
replace countrynameun="Libya" if countrynameun=="Libyan Arab Jamahiriya"
replace countrynameun="Bahrain" if countrynameun=="Bahrain (Kingdom of)"
replace countrynameun="Bolivia (Plurinational State of)" if countrynameun=="Bolivia"
replace countrynameun="Côte D'Ivoire" if countrynameun=="CÃ´te d'Ivoire"
merge m:1 countrynameun using  "C:\Users\stepharp\Desktop\FCTC\Country Codes and Sources\name_number_wbcode_matrix.dta"
list countrynameun if _merge==1
drop if countrynameun=="Cook Islands"
drop if countrynameun=="Niue"
order year countrynameun matrix
replace matrix_id = 118 if countrynameun=="Nauru"
drop if _merge==2
drop _merge number tradecode
drop v1
tab countrynameun
save "C:\Users\stepharp\Desktop\FCTC\Implemenation data 10.20.14\imp_04142015.dta"

order year countrynameun matrix_id meanall meanallart meanallart50
save "C:\Users\stepharp\Desktop\FCTC\Implemenation data 10.20.14\imp_04142015.dta", replace
