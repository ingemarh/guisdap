function dum=t2ps(site,rc,p)
if nargin<2, rc=0; end
if nargin<3, p=0; end
t2psfile='t_to_ps.txt';
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
p_rep=400000;
if p
 ch_adcint=[.6];
 ch_filter={'w600d6.fir'};
 ch_f=[1];
 sig=find(td_ch==1 & td_am==2);
 td_t1(sig)=td_t1(sig)+100;
 site='P';
else
 ch_adcint=[25];
 ch_filter={'b14d375.fir'};
 ch_f=[500];
end
for f=1:length(ch_f)
 d=find(td_ch==ch_f(f));
 td_ch(d)=f;
end
name_expr='beata';
name_site=upper(site);
save_PS
