function dum=t2ps(site,rc,p)
t_to_ps=[
	0 480 1 1
	600 11710-100*15 2 1
	11710-100*15 11710 2 1];
p_offsetppd=0;
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
ch_adcint=[15];
ch_filter=[15];
ch_f=[450];
p_rep=16000;
name_expr='syisr26';
name_site=upper(site);
save_PS
