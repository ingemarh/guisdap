function dum=t2ps(site)
load(['t_to_ps.' site '.txt'])
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
p_rep=1274880;
p_offsetppd=0;
ch_adcint=[16 16 20 20];
ch_filter={'b31d240.fir' 'b31d240.fir' 'b25d300.fir' 'b25d300.fir'};
ch_f=[1 2 4 5];
for f=1:length(ch_f)
 d=find(td_ch==ch_f(f));
 td_ch(d)=f;
%dd=find(td_am(d)==2);
%td_t1(d(dd))=td_t1(d(dd))+ch_adcint(f)/2;
end
save(['hilde' site 'pat_PS'],'td_*','p_*','ch_*')
