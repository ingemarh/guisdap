function dum=t2ps(site,rc)
if nargin<2, rc=0; end
if rc==0
 apustr='';
else
 apustr=['_' int2str(rc)];
end
t2psfile=['t_to_ps.txt' apustr '.' site];
t_to_ps=load(t2psfile,'-ascii');
[td_t1,f]=sort(t_to_ps(:,1)');
td_t2=t_to_ps(f,2)';
td_am=t_to_ps(f,3)';
td_ch=t_to_ps(f,4)';
p_offsetppd=0;
if site=='V'
 ch_adcint=[12 12];
 ch_filter={'b30d180.fir' 'b30d180.fir'};
 ch_f=[6 9];
 p_rep=999936;
elseif site=='v'
 ch_adcint=[15 15];
 ch_filter={'b25d225.fir' 'b25d225.fir'};
 ch_f=[6 5.8];
 p_rep=514080;
elseif site=='l'
 ch_adcint=[5 5];
 ch_filter={'b75d75.fir' 'b75d75.fir'};
 ch_f=[500.3 500.4];
 p_rep=300000;
end
for f=1:length(ch_f)
 td_ch(find(td_ch==ch_f(f)))=f;
end
name_expr='tau7';
name_site=upper(site);
save_PS
