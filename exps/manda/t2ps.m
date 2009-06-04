function dum=t2ps(site,rc)
if nargin<2, rc=0; end
t2psfile='t_to_ps.txt';
if rc==0
 apustr='';
else
 apustr=['_' int2str(rc)];
end
t2psfile=['t_to_ps.txt' apustr '.' lower(site)];
t_to_ps=load(t2psfile,'-ascii');
name_expr='manda';
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
if site=='L'
 if rc==3
  ch_adcint=2;
  ch_filter={'b170d30.fir'};
 else
  ch_adcint=5;
  ch_filter={'b100d75.fir'};
 end
 ch_f=500;
else
 ch_adcint=3;
 ch_filter={'b170d45.fir'};
 if site=='V'
  ch_f=9;
 else
  ch_f=12;
 end
end
p_rep=240000;
p_offsetppd=0;
for f=1:length(ch_f)
 td_ch(find(td_ch==ch_f(f)))=f;
end
name_expr='manda';
name_site=upper(site);
name_site=site;
save_PS
