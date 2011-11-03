function dum=t2ps(site,rc,p)
if nargin<2, rc=0; end
if nargin<3, p=0; end
if rc==0
 apustr='';
else
 apustr=['_' int2str(rc)];
end
t2psfile=['t_to_ps.txt' apustr '.' lower(site)];
t_to_ps=load(t2psfile,'-ascii');
p_offsetppd=0;
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
if p
 ch_adcint=[.6];
 ch_filter={'w600d6.fir'};
 ch_f=[1];
 sig=find(td_ch==1 & td_am==2);
 td_t1(sig)=td_t1(sig)+100;
 site='P';
 p_rep=400000;
elseif site=='l'
 ch_adcint=[25 25];
 ch_filter={'b14d375.fir' 'b14d375.fir'};
 ch_f=[500.3 500.5];
 p_rep=400000;
elseif site=='t'
 ch_adcint=[15 15];
 ch_filter={'b25d225.fir' 'b25d225.fir'};
 ch_f=[14 14.2];
 p_rep=720000;
elseif site=='r'
 ch_adcint=[15];
 ch_filter={'b25d225.fir'};
 ch_f=[14];
 p_rep=720000;
elseif site=='v'
 ch_adcint=[20 20];
 ch_filter={'b18d300.fir' 'b18d300.fir'};
 ch_f=[12 12.2];
 p_rep=357120;
else
 error('giveup')
end
for f=1:length(ch_f)
 d=find(td_ch==ch_f(f));
 td_ch(d)=f;
end
name_expr='bella';
name_site=upper(site);
save_PS
