% globals: global definitions for the data analysis
% GUISDAP v.1.81 03-01-27 Copyright EISCAT, Huuskonen&Lehtinen
%
% This script defines global variables needed in the data analysis
%
% See also: glob_GUP, start_GUP

% Define first the global variables used in the initialization
glob_GUP

% These globals are needed only in the analysis
 
global a_priori a_priorierror p_ND

global ad_range ad_w ad_code ad_lpg ad_coeff ADDR_SHIFT

global ch_az ch_el ch_f ch_Pt ch_scangle ch_range

global d_data d_parbl d_rcprog d_time d_filelist
global d_var1 d_var2 % sig_var1 sig_var2

global a_addr a_adstart a_adend a_control a_ind a_Magic_const
global a_NCAR a_save a_code a_intfixed a_intfix a_intallow a_intfixforce a_savespec a_txpower a_ppshortlags a_gating
global a_year a_start a_integr a_integdeffile a_skip a_end a_realtime a_txlim
global di_figures di_results di_spectra
global name_ant calTemp sysTemp
global webfile local a_autodir, webfile=cell(1);

global lpg_womscaled k_radar

% global p_coeff0 p_coeffg f_womega

 global r_range r_param r_dp r_error r_res r_status
 global r_apriori r_apriorierror r_h

% Global definitions for the spectrum calculations
 global pldfvv pldfv GUP_iniver
