% design: Executes the design package commands from a file
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% Main program in the experiment design package. The routine initializes the radar parameters
% and then calls the user program, which defines the transmission, reception, and lag profile
% parameters. A new experiment is specified by the following steps:
% 1) Make a new directory under directory exps, e.g. exps/CPXX
% 2) Write the design code into file CPXXT_design.m (here T is the site indicator)
% 3) Run DS_go and init_GUP 
%
% DS_start : defines the radar properties
% DS_set   : set a value to the user specified design variables
% DS_go    : transmission, reception and lag profile definitions
% DS_get   : enquire the value of internal design variables
%
% See also: DS_start DS_main
%

glob_design
cd(path_expr)
if exist('N_rcprog')~=1, N_rcprog=1; end
for d_rcprog=1:N_rcprog
  apustr=['_',int2str(d_rcprog)];
  if exist([path_expr,name_expr,name_site,apustr,'design.m'])==2
    file=canon([name_expr,name_site,apustr,'design']);
  elseif d_rcprog==1 & exist([path_expr,name_expr,name_site,'design.m'])==2
    file=canon([name_expr,name_site,'design']);
  elseif d_rcprog==1 & exist([path_expr,name_expr,name_site,'_design.m'])==2
    file=canon([name_expr,name_site,'_design']);
  else
   file=canon([path_expr,name_expr,name_site,apustr,'design']);
   fprintf(['Could not find desing file ', file,'\n'])
    error(' Error in design')
  end

  Stime=clock;

  eval(file)

  COR_end
  save_GUPvar
  fprintf('  Time used in initialization:%8.2f min\n',etime(clock,Stime)/60)
  fprintf('\n*****************************************************\n')
  fprintf('*\n*\n* Execute plot_td to see the timing diagram\n')
  fprintf('*\n*\n******************************************************\n')
end
