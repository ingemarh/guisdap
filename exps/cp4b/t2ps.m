function t2ps(name_site)
global local
[dum,name_expr]=fileparts(pwd);
eval(['load t_to_ps.txt.' name_site])
ll=find(t_to_ps(:,4)~=0); lt=size(t_to_ps,1);
t_to_ps=[t_to_ps;t_to_ps(ll,:)]; t_to_ps(lt+1:end,4)=t_to_ps(ll,4)+1;
td_t1=t_to_ps(:,1);
td_t2=t_to_ps(:,2);
td_am=t_to_ps(:,3);
td_ch=t_to_ps(:,4);
p_rep=14000;
p_offsetppd=0;
ch_adcint=[15 15 15 15];
ch_filter={'b15d225.fir' 'b15d225.fir' 'b15d225.fir' 'b15d225.fir'};
ch_f=[8 6 9 7];
for f=1:length(ch_f)
 td_ch(find(td_ch==ch_f(f)))=f;
end
save_PS
