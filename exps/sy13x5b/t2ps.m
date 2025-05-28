function dum=t2ps(site,rc,p)
t_to_ps=[
	0 5 1 1
	5 7 -1 1
	7 9 1 1
	9 10 -1 1
	10 11 1 1
	11 12 -1 1
	12 13 1 1
	66+[0 1800] 2 1
	1900+[0 99] 2 1];
baud=5;
t_to_ps(1:7,1:2)=t_to_ps(1:7,1:2)*baud;
p_offsetppd=0;
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
ch_adcint=0.25;
ch_filter={'CIC_127_80e6_20e5_d4'};
ch_f=430;
p_rep=2000;
name_expr='sy13x5b';
name_site=upper(site);
save_PS
