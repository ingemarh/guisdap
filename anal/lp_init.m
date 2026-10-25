function lp_init(expr,site,vers,tx,rx,bac,cal,rep,adc,filter,freq,mlag,offp)
% Generate init for simple longpulse exps
% tx [start stop]
% rx [start stop]
% bac [start stop]
% cal [start stop]
% rep us
% adc us
% filter
% freq MHz
% mlag maxlag
% offp offsetppd (only for tx site sofar)
% function lp_init(expr,site,vers,tx,rx,bac,cal,rep,adc,filter,freq,mlag)
global GUP_ver path_GUP path_exps name_expr name_site local
global td_ch td_t1 td_t2 td_am p_rep ch_f ch_filter ch_adcint p_offsetppd
globals
if nargin==1
 offp=expr{13};mlag=expr{12};freq=expr{11};filter=expr{10};adc=expr{9};
 rep=expr{8};cal=expr{7};bac=expr{6};rx=expr{5};tx=expr{4};vers=expr{3};
else
 name_expr=expr; name_site=upper(site);
end
if contains('IO',name_site)
 nsite=name_site;
 name_site='R'; txrem='N';
end
if ~exist('init_KST')
 addpath(fullfile(path_GUP,'init'))
end

p_offsetppd=offp;
t_to_ps=[tx 1 1;rx+offp 2 1;bac+offp 2 1];
if ~isempty(cal)
 t_to_ps=[t_to_ps;cal+offp 1 0;cal+offp 2 1];
else
 nocal=1;
end
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
if isempty(rep)
 rep=max(td_t2);
end
p_rep=rep;
p_dtau=1;
ch_adcint=adc;
if ischar(filter)
 ch_filter={filter};
else
 if filter<2, p_dtau=0.5; end
 ch_filter=filter;
end
ch_f=freq;
cwd=pwd;
%save_PS
if ~isempty(cal)
 LPgen={sprintf('COR_init(%d,1)',(mlag+1)*2+1),...
	sprintf('COR_uprog(0,1,1,''s'',1,%.0f,(0:%d)*%d,0,1)',diff(rx)/adc,mlag,adc),...
	sprintf('COR_uprog([],1,1,''b'',1,%.0f,(0:%d)*%d,0,1)',diff(bac)/adc,mlag,adc)...
	sprintf('COR_pp([],1,1,''c'',1,%.0f,0,1)',diff(cal)/adc)};
%elseif txrem=='N'
% LPgen={sprintf('COR_init(%d,1)',(mlag+1)*2),...
%	sprintf('COR_uprog(%d,1,1,''s'',1,%.0f,(1:%d)*%d,0,1)',diff(rx)/adc,diff(rx)/adc,mlag,adc),...
%	sprintf('COR_uprog([],1,1,''b'',1,%.0f,(0:%d)*%d,0,1)',diff(bac)/adc,mlag,adc),...
%	sprintf('COR_pp([],1,1,''s'',1,%.0f,0,1)',(rep-rx(1))/adc)};
else
 LPgen={sprintf('p_dtau=%g;',p_dtau),...
	sprintf('COR_init(%d,1)',(mlag+1)*2),...
	sprintf('COR_uprog(0,1,1,''s'',1,%.0f,(0:%d)*%d,0,1)',diff(rx)/adc,mlag,adc),...
	sprintf('COR_uprog([],1,1,''b'',1,%.0f,(0:%d)*%d,0,1)',diff(bac)/adc,mlag,adc)};
end
N_rcprog=vers;B_rcprog=vers;initplots=0;evalc('init_KST');evalc('init_GUP');
%N_rcprog=vers;B_rcprog=vers;init_KST;init_GUP;
chdir(cwd)
if exist('nsite')
 name_site=nsite;
end
