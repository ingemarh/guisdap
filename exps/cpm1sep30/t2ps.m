function dum=t2ps(site)
apustr='';
ch_adcint=10;
ch_filter=[1;50];
ch_f=8;
name_site=upper(site);
if name_site=='T'
 td_t1=[550 1300 6700 6800 8400];
 td_t2=[1050 6595 8295 8295 9895];
 td_am=[1 2 1 2 2];
 td_ch=[1 1 0 1 1];
end
p_rep=9995;
p_offsetppd=0;
name_expr='cpm1sep30';
save_PS
