% fullinit: script for initializing some EISCAT CP-experiments
% GUISDAP v1.70
% script for initializing some EISCAT CP-experiments
t=clock;
save([path_tmp 'timing'],'t')
delete([path_tmp 'fullinit.diary'])
diary([path_tmp 'fullinit.diary'])

%start_GUP,name_expr='CP1K'; name_site='T';N_rcprog=1;init_KST;init_GUP
%start_GUP,name_expr='CP1K'; name_site='R';N_rcprog=1;init_KST;init_GUP 
%start_GUP,name_expr='CP1H'; name_site='T';N_rcprog=1;init_KST;init_GUP 
%start_GUP,name_expr='CP1H'; name_site='R';N_rcprog=1;init_KST;init_GUP 
%start_GUP,name_expr='CP3F'; name_site='T';N_rcprog=6;init_KST;init_GUP 
%start_GUP,name_expr='CP3F'; name_site='R';N_rcprog=6;init_KST;init_GUP 
%start_GUP,name_expr='CP4B'; name_site='V';N_rcprog=1;init_KST;init_GUP 
%start_GUP,name_expr='CP7E'; name_site='V';N_rcprog=1;init_KST;init_GUP 
start_GUP,name_expr='tau2'; name_site='R';N_rcprog=1;init_KST;init_GUP 

diary off
load([path_tmp 'timing'])
delete([path_tmp 'timing.mat'])
fprintf('Time used %.0f minutes\n',etime(clock,t)/60)
