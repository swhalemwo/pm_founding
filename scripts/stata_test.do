/* * stat testing */

/* ** do the stata re tutorial */

clear
webuse nlswork

xtset

/* fixed effects */
xtreg ln_w grade age c.age#c.age ttl_exp c.ttl_exp#c.ttl_exp tenure c.tenure#c.tenure 2.race not_smsa south, fe

list in 1/6 /* list first 6 rows */
ds /* list variables */

/* random effect */
xtreg ln_w grade age c.age#c.age ttl_exp c.ttl_exp#c.ttl_exp tenure c.tenure#c.tenure 2.race not_smsa south, re

/* descriptives: within and between sd */
xtsum hours

/* msp: married and spouse present in household */
xttab msp
/* overall: person years */
/* between: 66% are at some point msp, 77% are not -> some women change */
/* within: if a woman is ever (in any of her obs) smp, 75% (55%?) of her obs are msp observations; if woman is ever not msp, 62% (72%?) of her obs are not msp: of all the women how are at some point not married, 62% (72%?) of their nobs are not married /*

xttrans msp


/* automated */

clear
import delimited /home/johannes/Dropbox/phd/papers/org_pop/data/processed/df_scl.csv

xtset iso3c_num year

xtnbreg nbr_opened hnwi_nbr_30m gptinc992j ghweal992j tmitr_approx_linear_2020step ti_tmitr_interact smorc_dollar_fxm nygdppcapcdk sppoptotlm clctr_cnt_cpaer sum_core cnt_contemp_1995, re


estat vce, corr

mata: b=st_matrix("e(b)")' 
 mata: st_matrix("b_stata", b)
mata: se=sqrt(diagonal(st_matrix("e(V)"))) 
 mata: st_matrix("se_stata", se)
matrix gof = ( e(N), e(ll), e(N_g), e(chi2), e(p), e(df_m))'
matrix stata_return = (b_stata', se_stata', gof')
matrix colnames stata_return = r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r20 r21 r22 r23 r24 r25 r26 r27 r28 r29 r30 r31 r32 r33 r34
svmat stata_return
 keep stata_return* 
 drop if missing(stata_return1)



save myauto



/* ** actually use my df */
clear
import delimited /home/johannes/Dropbox/phd/papers/org_pop/data/processed/df_scl.csv

list in 1/6

xtset iso3c_num year

xtsum nbr_opened

/* have to change some variable names (no capitals, no dots) */

/* negative binomial re*/

xtnbreg nbr_opened hnwi_nbr_30m gptinc992j ghweal992j tmitr_approx_linear_2020step ti_tmitr_interact smorc_dollar_fxm nygdppcapcdk sppoptotlm clctr_cnt_cpaer sum_core cnt_contemp_1995, re

estimates store negbin

/* testing to save */

matrix b_wide = e(b)
matrix b = b_wide'

matrix s = (  _se[gptinc992j], _se[hnwi_nbr_30m])

/* https://stackoverflow.com/questions/23955939/saving-coefficients-and-standard-errors-as-variables */
/* https://www.stata.com/statalist/archive/2013-04/msg00185.html */
mata: b=st_matrix("e(b)")'
mata: st_matrix("b_stata", b)
mata: se=sqrt(diagonal(st_matrix("e(V)")))
mata: st_matrix("se_stata", se)
getmata se, force

local my_matrix_rownames : rowfullnames b
display `my_matrix_rownames'

scalar asdfj = _se[cnt_contemp_1995]

scalar cnt_contemp_1995_se =  _se[cnt_contemp_1995]

matrix gof = ( e(N) \ e(ll) \ e(N_g) \ e(chi2) \ e(p) \ e(df_m))

matrix cbn = b_stata \ se_stata \ gof

svmat cbn
keep *r



/* put the coefs in the data */
svmat k
/* throw out everything that doesn't start with k */
keep k* 
save myauto

estat 

predict Fitted, xb
predict Epsilon2, deviance replace


/* Scatter plot */
scatter Epsilon Fitted 
graph export mygraph.pdf, replace

summarize nbr_opened Fitted, var



scatter Fitted





/* poisson re*/

xtpoisson nbr_opened hnwi_nbr_30m gptinc992j ghweal992j tmitr_approx_linear_2020step ti_tmitr_interact smorc_dollar_fxm nygdppcapcdk sppoptotlm clctr_cnt_cpaer sum_core cnt_contemp_1995, re

estimates store poisson

lrtest poisson negbin, force
/* p = 0.0435, just under 0.05.. :( */


estimates table negbin poisson, star(.05 .01 .001)

/* converges really quick huh */
estimates table, star(.05 .01 .001)

/* see if xtpoisson also drops groups with all zero outcome: yes it does */
xtpoisson nbr_opened hnwi_nbr_30m gptinc992j ghweal992j tmitr_approx_linear_2020step ti_tmitr_interact smorc_dollar_fxm nygdppcapcdk sppoptotlm clctr_cnt_cpaer sum_core cnt_contemp_1995, fe


/* crossed fe */
xtpoisson nbr_opened hnwi_nbr_30m gptinc992j ghweal992j tmitr_approx_linear_2020step ti_tmitr_interact smorc_dollar_fxm nygdppcapcdk sppoptotlm clctr_cnt_cpaer sum_core cnt_contemp_1995 i.year, fe
/* stata drops time-invariant vars for also when year fe (i.year) are added */




/* ** export */

eststo negbin
esttab negbin using "/home/johannes/Dropbox/phd/papers/org_pop/data/processed/stata_output"










/* ** pglm example for SO */

clear
import delimited /home/johannes/Dropbox/phd/papers/org_pop/data/processed/PatentsRDUS.csv

gen logrd = log(rd)
gen logcapital72 = log(capital72)


gen scisect2 = 0
replace scisect2 = 1 if scisect=="yes"

xtset cusip year

xtnbreg patents logrd scisect2 logcapital72, re
estimates table, star(.05 .01 .001)




/* ** scrap */

/* negbin fe, doesn't converge? */
/* also much stuff dropped because of all zeroes DV */

xtnbreg nbr_opened hnwi_nbr_30m gptinc992j ghweal992j tmitr_approx_linear_2020step ti_tmitr_interact smorc_dollar_fxm nygdppcapcdk sppoptotlm clctr_cnt_cpaer sum_core cnt_contemp_1995, fe

estimates store model3


while 1==1 {
ds
}
