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






/* ** actually use my df */
clear
import delimited /home/johannes/Dropbox/phd/papers/org_pop/data/processed/df_scl.csv

list in 1/6

xtset iso3c_num year

xtsum nbr_opened

/* have to change some variable names (no capitals, no dots) */

/* negative binomial re*/

xtnbreg nbr_opened hnwi_nbr_30m gptinc992j ghweal992j tmitr_approx_linear_2020step ti_tmitr_interact smorc_dollar_fxm nygdppcapcdk sppoptotlm clctr_cnt_cpaer sum_core cnt_contemp_1995, re

estimates store model1

/* poisson re*/

xtpoisson nbr_opened hnwi_nbr_30m gptinc992j ghweal992j tmitr_approx_linear_2020step ti_tmitr_interact smorc_dollar_fxm nygdppcapcdk sppoptotlm clctr_cnt_cpaer sum_core cnt_contemp_1995, re

estimates store model2

xtnbreg nbr_opened hnwi_nbr_30m gptinc992j ghweal992j tmitr_approx_linear_2020step ti_tmitr_interact smorc_dollar_fxm nygdppcapcdk sppoptotlm clctr_cnt_cpaer sum_core cnt_contemp_1995, re


estimates table model1 model2, star(.05 .01 .001)

/* converges really quick huh */
estimates table, star(.05 .01 .001)

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
