function dum=t2ps(site,rc,p)
name_expr='syisr32';
t2psfile='sy16ac_h_t2ps.txt';
t_to_ps=load(t2psfile,'-ascii');
p_offsetppd=0;
d=find(t_to_ps(:,3)==2);
t_to_ps=[t_to_ps;t_to_ps(d,:)+ones(size(d))*[8000 2000 0 0]];
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
ch_adcint=[10];
ch_filter=[10];
ch_f=[450];
p_rep=16000*82*32;
for f=1:length(ch_f)
 d=find(td_ch==ch_f(f));
 td_ch(d)=f;
end

name_site=upper(site);
save_PS
