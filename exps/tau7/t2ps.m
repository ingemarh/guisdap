function dum=t2ps(site)
eval(['load t_to_ps.txt.' site])
[td_t1,f]=sort(t_to_ps(:,1)');
td_t2=t_to_ps(f,2)';
td_am=t_to_ps(f,3)';
td_ch=t_to_ps(f,4)';
p_rep=999936;
p_offsetppd=0;
if site=='V'
 ch_adcint=[12 12];
 ch_filter={'b30d180.fir' 'b30d180.fir'};
 ch_f=[6 9];
else
end
for f=1:length(ch_f)
 td_ch(find(td_ch==ch_f(f)))=f;
end
clear t_to_ps f
eval(['save tau7' site 'pat_PS'])
