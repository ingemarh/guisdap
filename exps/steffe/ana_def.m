% Analysis defaults
a_satch.clutter=[32 32 16];
a_satch.sigma=4;
%a_satch.plot=8;
%a_satch.repair=-Inf;
if strfind(data_path,'32p')
 analysis_do=0;
 analysis_txpower=8;
 d_saveintdir=result_path;
 d=strfind(d_saveintdir,'AUTO');
 if length(d)==1
  d_saveintdir=[d_saveintdir(1:d-1) sprintf('%s_%s_%d@32p',datestr(analysis_start,29),name_expr,analysis_integr(1))];
 end
end
