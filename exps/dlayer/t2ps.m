function dum=t2ps(site)
eval(['load t_to_ps.txt.' site])
if site=='L'
 p_rep=120000;
 ch_filter={'w125d40.fir'};
 ch_f=[0];
else
 p_rep=174800;
 ch_filter={'b150d60.fir'};
 ch_f=[8];
end
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
p_offsetppd=0;
ch_adcint=[4];
if site=='L'
 d=find(rem(td_t1,1200)==1101); td_ch(d)=1;
end
for f=1:length(ch_f)
 td_ch(find(td_ch==ch_f(f)))=f;
end
if site=='L'
 td_ch(d)=0;
end

eval(['save dlayer' site 'pat_PS ch_* p_* td_*'])
