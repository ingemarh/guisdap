function dum=t2ps(site)
load(['t_to_ps.txt.' site])
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
p_rep=357120;
p_offsetppd=0;
ch_adcint=[12 12 10];
ch_filter={'b42d180.fir' 'b42d180.fir' 'b25d150.fir'};
ch_f=[13 14 4];
if site=='R'
 ch_adcint=[12 12];
 ch_filter={'b42d180.fir' 'b42d180.fir'};
 ch_f=[13 14];
end
for f=1:length(ch_f)
 d=find(td_ch==ch_f(f));
 td_ch(d)=f;
 dd=find(td_am(d)==2);
 td_t1(d(dd))=td_t1(d(dd))+ch_adcint(f)/2;
end
save(['tau2pl' site 'pat_PS'],'td_*','p_*','ch_*')
