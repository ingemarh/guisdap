function dum=t2ps(rcno)
if nargin<1, rcno=[]; end
if rcno=='r'
 load t_to_ps.txt.r
 ch_filter={'b13d420.fir' 'b13d420.fir'};
 ch_adcint=[28 28];
 p_offsetppd=-1;
 ch_f=[6 10];
 name_site='R';
else
 load(['t_to_ps.txt' num2str(rcno)])
 ch_filter={'b25d210.fir' 'b25d210.fir'};
 ch_adcint=[14 14];
 p_offsetppd=0;
 ch_f=[6 8];
 name_site='V';
end
apustr='';
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
for f=1:length(ch_f)
 td_ch(find(td_ch==ch_f(f)))=f;
end
p_rep=714112;
if rcno=='r'
 ch_f=[6 8];
end
name_expr='tau8';
save_PS
