function dum=t2ps(site,rc,p)
t_to_ps=[];
p_offsetppd=-800;
if site=='3'
  t_to_ps=[ 0 480 1 1 ];
  p_offsetppd=0;
end
t_to_ps=[ t_to_ps
	600+[0 639]*10 2 1
	600+640*10+[0 99]*10 2 1];
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
ch_adcint=10;
ch_filter={'CIC_127_80e6_50e3_d160'};
ch_f=440;
p_rep=14000;
name_expr='sy480l';
name_site=upper(site);
save_PS
