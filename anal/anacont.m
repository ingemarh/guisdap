function anacont(no_gfd)
% Continue an interrupted (gfd) analysis
global path_GUP path_exps path_tmp name_expr name_site data_path result_path
global a_integr a_txlim a_control a_interval a_year a_realtime a_end a_skip a_Magic_const a_NCAR a_code a_save a_ind
global a_classic
global di_figures di_results d_saveintdir

if isempty(a_integr)
  error('Cannot continue analysis, please start from scratch')
end
analysis_start=toYMDHMS(a_year,a_interval(1));
analysis_end=toYMDHMS(a_year,a_end);
if analysis_end(2)>12, analysis_end=[a_year 12 31 24 0 0]; end
analysis_integr=a_integr; analysis_skip=a_skip;
a_ind=a_ind-1; if a_ind==0, a_ind=length(a_integr); end
analysis_txlimit=a_txlim; analysis_realtime=a_realtime;
Magic_const=a_Magic_const; NCAR=a_NCAR;
analysis_code=a_code; analysis_save=a_save;
analysis_control=a_control; analysis_classic=a_classic;
display_figures=di_figures; display_results=di_results;

read_anapar
if nargin<1 | ~no_gfd
  name_expr_save=name_expr;
  data_path_save=data_path;
  result_path_save=result_path;
  load([path_tmp '.gup'],'-mat')
  name_expr=name_expr_save;
  data_path=data_path_save;
  result_path=result_path_save;
  for i=1:size(extra,1),eval(extra(i,:));end
end
an_start
