function dum=t2ps(site)
load t_to_ps.txt.R
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
p_rep=454144;
p_offsetppd=0;
ch_adcint=[4];
%ch_adcint=[12 122];
ch_filter={'b150d60.fir'};
%ch_filter={'b42d180.fir' 'b42d180.fir'};
ch_f=[14];
%ch_f=[13 14];
for f=1:length(ch_f)
 td_ch(find(td_ch==ch_f(f)))=f;
end
clear t_to_ps f
save arcRpat_PS
%save tau2Rpat_PS
