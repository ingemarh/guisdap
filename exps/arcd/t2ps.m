function dum=t2ps(site)
eval(['load t_to_ps.txt.' site])
if site=='V'
 p_rep=172800;
 ch_filter={'b250d30.fir'};
else
end
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
p_offsetppd=0;
ch_adcint=[2];
ch_f=[8];
for f=1:length(ch_f)
 td_ch(find(td_ch==ch_f(f)))=f;
end

eval(['save arcd' site 'pat_PS ch_* p_* td_*'])
