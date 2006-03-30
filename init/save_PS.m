% save_patPS.m: saves experiment setup
% GUISDAP v.8.4 04-11-27 Copyright EISCAT
%
% These variables are need in the initialisation
%
% See also: save_GUPvar save_toinitfile

if ~exist('local','var')
 global local
end

if ~exist('apustr'), apustr=''; end
PSfile=[path_expr name_expr name_site apustr 'pat_PS'];
saveflag='';
if local.matlabversion>=7
 saveflag='-v6';
end

save(PSfile,'p_*','td_*','ch_*',saveflag);
disp([PSfile ' saved']);

clear PSfile
