function t2ps(site,rc,p)
name_expr='sy32x20a';
if rc==1
 apustr='';
 p_rep=14000*64;
 apustr='';
elseif rc==9
 apustr=['_' num2str(rc)];
 p_rep=2000*64;
end
t2psfile=['sy32ac' apustr '_' site' '_t2ps.txt'];
ch_filter={'CIC_127_80e6_50e3_d160'};
t_to_ps=load(t2psfile,'-ascii');
p_offsetppd=0;
d=find(t_to_ps(:,3)==2);
t_to_ps=[t_to_ps;t_to_ps(d,:)+ones(size(d))*[1 2 0 0]];
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
ch_adcint=10;
ch_f=430;
for f=1:length(ch_f)
 d=find(td_ch==ch_f(f));
 td_ch(d)=f;
end
if site=='r'
 if rc==1
  p_offsetppd=-800;
 elseif rc==9
  p_offsetppd=-10;
 end
end

name_site=upper(site);
save_PS
