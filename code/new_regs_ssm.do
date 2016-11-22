log using c:\misc\fctc\final_regs_ssm2, t

# delim ;
 logit  adopt i.t i.who_reg2 SP_POP_TOTL fh_ipolity2 tobac_prod perc_male perc_female   
 SL_TLF_TOTL_FE_ZS ciri_wopol_23 NY_GDP_PCAP_KD ngo_FCA_const   diaginbfctc_sq gltotal_sq  
 in_deg1 in_deg2 in_deg3 in_deg12 in_deg13 in_deg14 exp_1 exp_2 exp_3 exp_12 exp_13 exp_14 
 if conv==1  & year_bak<2012, or cluster(id)  ;  
/* excluding US and Suisse */ 
# delim ;
 logit  adopt i.t i.who_reg2 SP_POP_TOTL fh_ipolity2 tobac_prod perc_male perc_female   
 SL_TLF_TOTL_FE_ZS ciri_wopol_23 NY_GDP_PCAP_KD ngo_FCA_const   diaginbfctc_sq  gltotal_sq
 in_deg1 in_deg2 in_deg3 in_deg12 in_deg13 in_deg14 exp_1 exp_2 exp_3 exp_12 exp_13 exp_14 
 if id!=165 & id!=183 & conv==1  & year_bak<2012, or cluster(id)  ;  
/* no degree scores */
# delim ;
 logit  adopt i.t i.who_reg2 SP_POP_TOTL fh_ipolity2 tobac_prod perc_male perc_female   
 SL_TLF_TOTL_FE_ZS ciri_wopol_23 NY_GDP_PCAP_KD ngo_FCA_const   diaginbfctc_sq  gltotal_sq
 exp_1 exp_2 exp_3 exp_12 exp_13 exp_14 
 if  conv==1  & year_bak<2012, or cluster(id)  ;  
/* including outdegree instead of indegree */ 
# delim ;
 logit  adopt i.t i.who_reg2 SP_POP_TOTL fh_ipolity2 tobac_prod perc_male perc_female   
 SL_TLF_TOTL_FE_ZS ciri_wopol_23 NY_GDP_PCAP_KD ngo_FCA_const   diaginbfctc_sq  gltotal_sq
 out_deg1 out_deg2 out_deg3 out_deg12 out_deg13 out_deg14 exp_1 exp_2 exp_3 exp_12 exp_13 exp_14 
 if  conv==1  & year_bak<2012, or cluster(id)  ;  
/* for GL members only */ 
# delim ;
 logit  adopt i.t i.who_reg2 SP_POP_TOTL fh_ipolity2 tobac_prod perc_male perc_female   
 SL_TLF_TOTL_FE_ZS ciri_wopol_23 NY_GDP_PCAP_KD ngo_FCA_const   diaginbfctc_sq  gltotal_sq
 in_deg1 in_deg2 in_deg3 in_deg12 in_deg13 in_deg14 exp_1 exp_2 exp_3 exp_12 exp_13 exp_14 
 if conv==1 & GL_total_members_robert>0 & year_bak<2012, or cluster(id)  ;  
/* Dichotomized exposure */ 
# delim ;
 logit  adopt i.t i.who_reg2 SP_POP_TOTL fh_ipolity2 tobac_prod perc_male perc_female   
 SL_TLF_TOTL_FE_ZS ciri_wopol_23 NY_GDP_PCAP_KD ngo_FCA_const   diaginbfctc_sq  gltotal_sq
 out_deg1 out_deg2 out_deg3 out_deg12 out_deg13 out_deg14 exp_1 exp_2 exp_3 exp_12d exp_13d exp_14d 
 if conv==1  & year_bak<2012, or cluster(id)  ;  
/* GL only Dichotomized exposure */ 
# delim ;
 logit  adopt i.t i.who_reg2 SP_POP_TOTL fh_ipolity2 tobac_prod perc_male perc_female   
 SL_TLF_TOTL_FE_ZS ciri_wopol_23 NY_GDP_PCAP_KD ngo_FCA_const   diaginbfctc_sq  gltotal_sq
 in_deg1 in_deg2 in_deg3 in_deg12 in_deg13 in_deg14 exp_1 exp_2 exp_3 exp_12d exp_13d exp_14d 
 if conv==1 & GL_total_members_robert>0 & year_bak<2012, or cluster(id)  ;  /* Random exposure */ 
# delim ;
 logit  adopt i.t i.who_reg2 SP_POP_TOTL fh_ipolity2 tobac_prod perc_male perc_female   
 SL_TLF_TOTL_FE_ZS ciri_wopol_23 NY_GDP_PCAP_KD ngo_FCA_const   diaginbfctc_sq  gltotal_sq
 out_deg1 out_deg2 out_deg3 out_deg12 out_deg13 out_deg14 exp_1 exp_2 exp_3 exp_20
 if conv==1  & year_bak<2012, or cluster(id)  ;  

# delim ;
/* external influence over time NGO FCA */ 
logit  adopt i.t i.who_reg2 SP_POP_TOTL fh_ipolity2 tobac_prod perc_male perc_female   
 SL_TLF_TOTL_FE_ZS ciri_wopol_23 NY_GDP_PCAP_KD ngo_FCA_const   diaginbfctc_sq  gltotal_sq
 in_deg1 in_deg2 in_deg3 in_deg12 in_deg13 in_deg14 exp_1 exp_2 exp_3 exp_12 exp_13 exp_14 
 year_NGO if conv==1  & year_bak<2012, or cluster(id)  ;   
/* influence over time */ 
logit  adopt i.t i.who_reg2 SP_POP_TOTL fh_ipolity2 tobac_prod perc_male perc_female   
  SL_TLF_TOTL_FE_ZS ciri_wopol_23 NY_GDP_PCAP_KD ngo_FCA_const   diaginbfctc_sq  gltotal_sq
  in_deg1 in_deg2 in_deg3 in_deg12 in_deg13 in_deg14 exp_1 exp_2 exp_3 exp_12 exp_13 exp_14 
  year_exp14 if conv==1  & year_bak<2012, or cluster(id)  ;   
/* OL influence at time=3 & year_bak<2012,3 */ 
logit  adopt i.t i.who_reg2 SP_POP_TOTL fh_ipolity2 tobac_prod perc_male perc_female   
 SL_TLF_TOTL_FE_ZS ciri_wopol_23 NY_GDP_PCAP_KD ngo_FCA_const   diaginbfctc_sq  gltotal_sq
 in_deg1 in_deg2 in_deg3 in_deg12 in_deg13 in_deg14 exp_1 exp_2 exp_3 expc_12 expc_13 expc_14 
 year34_expc14 if conv==1  & year_bak<2012, or cluster(id)  ;   

 /* selection */ 
oneway year select12_14g1   if adopt==1, tab;
# delim ;
reg  year i.who_reg2 SP_POP_TOTL fh_ipolity2 tobac_prod perc_male perc_female   
 SL_TLF_TOTL_FE_ZS ciri_wopol_23 NY_GDP_PCAP_KD ngo_FCA_const   diaginbfctc_sq  gltotal_sq
 in_deg1 in_deg2 in_deg3 in_deg12 in_deg13 in_deg14 exp_1 exp_2 exp_3 expc_12 expc_13 expc_14 
 select12_14g1  if conv==1  & year_bak<2012, b ;   


# delim ;
/* Thresholds Susceptibility & Infection */ 
glm thr_14   i.t i.who_reg2 SP_POP_TOTL fh_ipolity2 tobac_prod perc_male perc_female   
  SL_TLF_TOTL_FE_ZS ciri_wopol_23 NY_GDP_PCAP_KD ngo_FCA_const  
  diaginbfctc_sq   gltotal_sq 
  in_deg1 in_deg2 in_deg3 in_deg12 in_deg13 in_deg14 exp_1 exp_2 exp_3 exp_12 exp_13 exp_14 
  if  conv==1 & adopt==1 & t<11 & year_bak<2012, link(logit) family(binomial) robust nolog;
glm suscep_1n_n14  i.t i.who_reg2 SP_POP_TOTL fh_ipolity2 tobac_prod perc_male perc_female   
  SL_TLF_TOTL_FE_ZS ciri_wopol_23 NY_GDP_PCAP_KD ngo_FCA_const  
  diaginbfctc_sq   gltotal_sq
  in_deg1 in_deg2 in_deg3 in_deg12 in_deg13 in_deg14 exp_1 exp_2 exp_3 exp_12 exp_13 exp_14
  if conv==1 & adopt==1 & t>1 & t<11 & year_bak<2012, link(logit) family(binomial) robust nolog;
# delim ;
glm infect_1n_n14  i.t i.who_reg2 SP_POP_TOTL fh_ipolity2 tobac_prod perc_male perc_female   
  SL_TLF_TOTL_FE_ZS ciri_wopol_23 NY_GDP_PCAP_KD ngo_FCA_const  
  diaginbfctc_sq   gltotal_sq
  in_deg1 in_deg2 in_deg3 in_deg12 in_deg13 in_deg14 exp_1 exp_2 exp_3 exp_12 exp_13 exp_14
  if conv==1 & adopt==1 & t<10 & year_bak<2012, link(logit) family(binomial) robust nolog; 
log close  ;

 
 # delim ;
glm suscep_1n_n14n  i.t i.who_reg2 SP_POP_TOTL fh_ipolity2 tobac_prod perc_male perc_female   
  SL_TLF_TOTL_FE_ZS ciri_wopol_23 NY_GDP_PCAP_KD ngo_FCA_const  
  diaginbfctc_sq   gltotal_sq
  in_deg1 in_deg2 in_deg3 in_deg12 in_deg13 in_deg14 exp_1 exp_2 exp_3 exp_12 exp_13 exp_14
  if conv==1 & adopt==1 , link(logit) family(binomial) robust nolog;
# delim ;
glm infect_1n_n14n  i.t i.who_reg2 SP_POP_TOTL fh_ipolity2 tobac_prod perc_male perc_female   
  SL_TLF_TOTL_FE_ZS ciri_wopol_23 NY_GDP_PCAP_KD ngo_FCA_const  
  diaginbfctc_sq   gltotal_sq
  in_deg1 in_deg2 in_deg3 in_deg12 in_deg13 in_deg14 exp_1 exp_2 exp_3 exp_12 exp_13 exp_14
  if conv==1 & adopt==1 , link(logit) family(binomial) robust nolog; 
 
 
 # delim ;
summ  suscep_1n_n14n  i.t i.who_reg2 SP_POP_TOTL fh_ipolity2 tobac_prod perc_male perc_female   
  SL_TLF_TOTL_FE_ZS ciri_wopol_23 NY_GDP_PCAP_KD ngo_FCA_const  
  diaginbfctc_sq   gltotal_sq
  in_deg1 in_deg2 in_deg3 in_deg12 in_deg13 in_deg14 exp_1 exp_2 exp_3 exp_12 exp_13 exp_14
  if conv==1  & adopt==1;
summ infect_1n_n14n  i.t i.who_reg2 SP_POP_TOTL fh_ipolity2 tobac_prod perc_male perc_female   
  SL_TLF_TOTL_FE_ZS ciri_wopol_23 NY_GDP_PCAP_KD ngo_FCA_const  
  diaginbfctc_sq   gltotal_sq
  in_deg1 in_deg2 in_deg3 in_deg12 in_deg13 in_deg14 exp_1 exp_2 exp_3 exp_12 exp_13 exp_14
  if conv==1  & adopt==1;  
 
 
 
 
 
 
 
 
 
 
# delim ;
 logit  adopt i.t i.who_reg2 SP_POP_TOTL fh_ipolity2 tobac_prod perc_male perc_female   
 SL_TLF_TOTL_FE_ZS ciri_wopol_23 NY_GDP_PCAP_KD ngo_FCA_const   diaginbfctc_sq  gltotal_sq
 in_deg1 in_deg2 in_deg3 in_deg12 in_deg13 in_deg14 exp_1 exp_2 exp_3 exp_12d exp_13d exp_14d 
  year_select14
 if conv==1  & year_bak<2012, or cluster(id)  ;  
 
# delim ;
 logit  adopt i.t normal i.who_reg2 SP_POP_TOTL fh_ipolity2 tobac_prod perc_male perc_female   
 SL_TLF_TOTL_FE_ZS ciri_wopol_23 NY_GDP_PCAP_KD ngo_FCA_const   diaginbfctc_sq  gltotal_sq
 in_deg1 in_deg2 in_deg3 in_deg12 in_deg13 in_deg14 exp_1 exp_2 exp_3 exp_12d exp_13d exp_14d 
  normal_exp14d
 if conv==1  & year_bak<2012, or cluster(id)  ;   
 
oneway selection14 t, tab
oneway ngo_FCA_c t, tab
oneway in_deg14 t, tab
oneway exp_14 t, tab
 
 