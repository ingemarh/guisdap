% Analysis defaults
a_satch.clutter=[32 32 16];
a_satch.sigma=4;
%a_satch.plot=8;
%a_satch.repair=-Inf;
if strfind(data_path,'32p')
 analysis_do=0;
 d_saveintdir=fullfile(path_tmp,sprintf('steffe_%s_int@32p',datestr(analysis_start,29)));
end
