function dum=t2ps(site)
eval(['load t_to_ps.txt.' site])
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
p_rep=454144;
p_offsetppd=0;
ch_adcint=[6];
ch_filter={'b83d90.fir'};
ch_f=[14];
for f=1:length(ch_f)
 td_ch(find(td_ch==ch_f(f)))=f;
end
clear t_to_ps f site
eval(['save arc' site 'pat_PS'])
