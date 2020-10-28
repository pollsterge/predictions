// This replicates nonresponse weighting method described in Valliant & Dever, "Survey Weights - A Step-By-Step Guide," https://www.stata.com/bookstore/survey-weights/

clear
clear matrix
clear mata
set maxvar 10000
use "omnibus_CRRC_Omnibus_Public_Wave5.dta" 

gen ownership=0
foreach x in 1 2 3 4 5 6 7 8 9 10 {
replace ownership = ownership + 1 if d3_`x'== 1
}

recode age (18/34=1)(35/54=2)(55/119=3), gen(agegroup)
recode c1 (-9/-1=4)
gen r_pq = 0
replace r_pq = 1 if (m5>0)
recode d7 (3=1)(.=.)(else=0), gen(relig)
recode m1 (1=1)(5=1)(6/9=1)(2/4=2)(12=2)(-5=3)(-4=4)(-2/-1=3)(.=.)(else=5),gen(tv)
recode m3 (1/4=1)(5=0)(-5/-1=0)(else=0),gen(internet)
recode d6 (3=1)(.=.)(else=0), gen(ethnic)
logit r_pq i.c1 i.agegroup i.tv internet

predict predp, pr

pctile qpreds=predp, nq(8) genp(percent)

sort qpreds

list qpreds if qpreds != .

egen pclass = cut(predp), at(0, 0.1563829, 0.2417719, 0.3043088, 0.4370272, 0.4734559, 0.5716624, 0.6139672, 1)

sort pclass

egen pavg = mean(predp), by(pclass)
egen pmed = median(predp), by(pclass)
egen RR = mean(r_pq), by(pclass)
egen pavgwtd = wtmean(predp), weight(wt) by(pclass)
egen RRwtd = wtmean(r_pq), weight(wt) by(pclass)
gen adj = 1/pavgwtd

table pclass, contents(mean pavg mean pavgwtd mean pmed mean RR mean RRwtd)

gen party = m5
replace party = . if party < 0
recode party (12=99)
replace party = 12 if m5_oth == 1
replace party =2 if m5_oth==9
replace party =2 if m5_oth==8
lab val party m5
lab def m5 12 "Solidarity Alliance", modify
lab def m5 99 "Other", modify
prop party[pw=adj]

/// Check the quality of predictions using ML technique


generate random = runiform()
sort random
gen intest = _n <= 242
gen intrain = _n > 242
preserve
keep if intest == 1
save omtest.dta, replace
restore
keep if intrain == 1
logit r_pq i.c1 i.agegroup i.tv internet //i.ethnic i.agegroup i.stratum i.sex i.c1 i.education i.relig i.tv internet ownership
use omtest, clear
predict testp, pr
gen testR=round(testp)
gen test_accuracy=testR==r_pq
tab test_accuracy
tab testR r_pq, col V
