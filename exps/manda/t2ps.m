function dum=t2ps(site)
eval(['load t_to_ps.txt.' lower(site)])
name_expr='manda';
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
p_rep=240000;
p_offsetppd=0;
ch_adcint=[3];
ch_filter={'b170d45.fir'};
ch_f=[9];
for f=1:length(ch_f)
 td_ch(find(td_ch==ch_f(f)))=f;
end
name_expr='manda';
name_site=upper(site);
save_PS
