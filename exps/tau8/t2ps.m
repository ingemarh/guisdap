function dum=t2ps(rcno)
if nargin<1, rcno=[]; end
load(['t_to_ps.txt' num2str(rcno)])
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
p_offsetppd=0;
ch_adcint=[14 14];
ch_filter={'b25d210.fir' 'b25d210.fir'};
ch_f=[6 8];
for f=1:length(ch_f)
 td_ch(find(td_ch==ch_f(f)))=f;
end
clear t_to_ps f
if rcno==2
 p_rep=714112;
 save tau8V_2pat_PS
else
 p_rep=833280;
 save tau8Vpat_PS
end
