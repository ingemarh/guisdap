function dum=t2ps(site)
eval(['load t_to_ps.txt.' site])
size(t_to_ps)
if site=='R'
 f=find(rem(t_to_ps(:,1),9765)==1970);
 t_to_ps=[t_to_ps;[t_to_ps(f,1)+100*7+20 t_to_ps(f,2)+20 t_to_ps(f,3:4)]];
 t_to_ps(f,2)=t_to_ps(f,1)+100*7;
end
size(t_to_ps)
[td_t1,f]=sort(t_to_ps(:,1)');
td_t2=t_to_ps(f,2)';
td_am=t_to_ps(f,3)';
td_ch=t_to_ps(f,4)';
p_rep=624960;
p_offsetppd=0;
if site=='T'
 ch_adcint=[10 10 7 7 10 10];
 ch_filter={'b25d150.fir' 'b25d150.fir' 'b75d105.fir' 'b75d105.fir' 'b25d150.fir' 'b25d150.fir'};
 ch_f=[14 15 7 11 12 13];
else
 ch_adcint=[10 10 7 7];
 ch_filter={'b25d150.fir' 'b25d150.fir' 'b25d105.fir' 'b25d105.fir'};
 ch_f=[14 15 7 11];
end
for f=1:length(ch_f)
 td_ch(find(td_ch==ch_f(f)))=f;
end
clear t_to_ps f
eval(['save cp1l' site 'pat_PS'])
