% glob_initKST.m: All the global variables needed to run init_EISCAT
% GUISDAP v1.60   96-05-27 Copyright Asko Huuskonen, Markku Lehtinen
%
%
% See also: init_EISCAT, globals, glob_GUP, start_GUP

global ch_f ch_p ch_adcint ch_filter ch_fradar ch_gain

global lp_t1 lp_t2 lp_dt lp_nt lp_vc lp_ra lp_ri lp_T lp_code lp_bcs lp_h lp_ind 
global lp_nfir lp_fir lp_dec 
 
global p_dtau  p_rep p_ND ra_prev ra_next
global p_XMITloc p_RECloc p_calTemp

global td_ch td_t1 td_t2 td_am p_rep

global vc_ch vc_t1 vc_t2 vc_adcint vc_p vc_env vc_envo vc_sampling vc_mf 
global vc_ba bm_next % included to  achieve compatibility with the experiment design system
