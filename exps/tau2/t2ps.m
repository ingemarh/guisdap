function dum=t2ps(site)
load t_to_ps.txt.T
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
p_rep=357120;
p_offsetppd=0;
ch_adcint=[12 12 2 2];
%ch_adcint=[12 122];
ch_filter={'b42d180.fir' 'b42d180.fir' 'b42d180.fir' 'b42d180.fir'};
%ch_filter={'b42d180.fir' 'b42d180.fir'};
ch_f=[13 14 11 9];
%ch_f=[13 14];
for f=1:length(ch_f)
 td_ch(find(td_ch==ch_f(f)))=f;
end
clear t_to_ps f
save tau2Tpat_PS
%save tau2Rpat_PS
