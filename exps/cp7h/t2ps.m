function dum=t2ps(name_site)
eval(['load t_to_ps.txt.' name_site])
td_t1=t_to_ps(:,1);
td_t2=t_to_ps(:,2);
td_am=t_to_ps(:,3);
td_ch=t_to_ps(:,4);
l=length(td_ch);
f=find(td_ch>0);
tdd_ch=td_ch;
for i=1:3
 tdd_ch(f)=rem(tdd_ch(f)-2-1,4)+6;
 t_to_ps=[t_to_ps;[td_t1+i*18465 td_t2+i*18465 td_am tdd_ch]];
end
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
p_rep=73860;
p_offsetppd=0;
ch_adcint=[15 15 15 15];
ch_filter={'b30d225.fir' 'b30d225.fir' 'b30d225.fir' 'b30d225.fir'};
ch_f=[6 7 8 9];
for f=1:length(ch_f)
 td_ch(find(td_ch==ch_f(f)))=f;
end
clear t_to_ps f l tdd_ch i
eval(['save cp7h' name_site 'pat_PS'])
