set more off
import delimited dfx_ncvrgd.csv

xtset iso3c_num year

xtnbreg nbr_opened indtaxincentives npotaxexemption cnt_contemp_1995 cnt_contemp_1995_squared hnwi_nbr_30m_lag4 sptinc992j_p99p100_lag3 shweal992j_p90p100_lag3 nygdppcapcdk_lag2 sppoptotlm_lag5 clctr_cnt_cpaer_lag2 nbr_opened_cum_lag1, re 

estimates store r_xtnbreg


menbreg nbr_opened indtaxincentives npotaxexemption cnt_contemp_1995 cnt_contemp_1995_squared hnwi_nbr_30m_lag4 sptinc992j_p99p100_lag3 shweal992j_p90p100_lag3 nygdppcapcdk_lag2 sppoptotlm_lag5 clctr_cnt_cpaer_lag2 nbr_opened_cum_lag1 || iso3c_num:

estimates store r_menbreg

xtreg nbr_opened indtaxincentives npotaxexemption cnt_contemp_1995 cnt_contemp_1995_squared hnwi_nbr_30m_lag4 sptinc992j_p99p100_lag3 shweal992j_p90p100_lag3 nygdppcapcdk_lag2 sppoptotlm_lag5 clctr_cnt_cpaer_lag2 nbr_opened_cum_lag1, re mle

estimates store r_xtreg

mixed nbr_opened indtaxincentives npotaxexemption cnt_contemp_1995 cnt_contemp_1995_squared hnwi_nbr_30m_lag4 sptinc992j_p99p100_lag3 shweal992j_p90p100_lag3 nygdppcapcdk_lag2 sppoptotlm_lag5 clctr_cnt_cpaer_lag2 nbr_opened_cum_lag1 || iso3c_num:

estimates store r_mixed

xtmixed nbr_opened indtaxincentives npotaxexemption cnt_contemp_1995 cnt_contemp_1995_squared hnwi_nbr_30m_lag4 sptinc992j_p99p100_lag3 shweal992j_p90p100_lag3 nygdppcapcdk_lag2 sppoptotlm_lag5 clctr_cnt_cpaer_lag2 nbr_opened_cum_lag1 || iso3c_num:

estimates store r_xtmixed



estimates table r_xtnbreg r_menbreg r_xtreg r_mixed r_xtmixed, stats(N ll chi2 aic)
