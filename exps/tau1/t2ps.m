function dum=t2ps(site)
eval(['load t_to_ps.txt.' site])
p_offsetppd=0;
if site=='V'
 ll=find(t_to_ps(:,4)~=0)+size(t_to_ps,1);
 t_to_ps=[t_to_ps;t_to_ps]; t_to_ps(ll,4)=t_to_ps(ll,4)+1;
 p_rep=998400;
 ch_adcint=[24 24 24 24];
 ch_filter={'b21d360.fir' 'b21d360.fir' 'b21d360.fir' 'b21d360.fir'};
 ch_f=[8 10 9 11];
else
 p_rep=714240;
 ch_adcint=[12 12];
 ch_filter={'b42d180.fir' 'b42d180.fir'};
 ch_f=[13 14];
end
[td_t1,f]=sort(t_to_ps(:,1)');
td_t2=t_to_ps(f,2)';
td_am=t_to_ps(f,3)';
td_ch=t_to_ps(f,4)';
for f=1:length(ch_f)
 td_ch(find(td_ch==ch_f(f)))=f;
end
eval(['save tau1' site 'pat_PS p_* td_* ch_*'])
