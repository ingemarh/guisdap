function dum=t2ps(site,rc)
name_expr='sy16x3a';
name_site=upper(site);
if nargin<2, rc=0; end
t2psfile=[name_expr '_' site '_t2ps.txt'];
if rc==0
 apustr='';
else
 apustr=['_' int2str(rc)];
end
%t2psfile=['t_to_ps.txt' apustr '.' lower(site)];
t_to_ps=load(t2psfile,'-ascii');
d=find(t_to_ps(:,3)==2);
t_to_ps=[t_to_ps;t_to_ps(d,:)+ones(size(d))*[1100 55 0 0]];
td_t1=t_to_ps(:,1)';
td_t2=t_to_ps(:,2)';
td_am=t_to_ps(:,3)';
td_ch=t_to_ps(:,4)';
p_rep=40000;
ch_f=430;
ch_filter={'CIC_127_80e6_2e6_d4'};
real_adc=0.25;
ch_adcint=1.5;
ch_decimation=[ch_adcint/real_adc 2]; % [adc_int/real_adc real_adc/p_dtau]
d=find(td_am==2);
td_t1(d)=td_t1(d)+ch_adcint;
if name_site=='3'
 p_offsetppd=0;
end
for f=1:length(ch_f)
 td_ch(find(td_ch==ch_f(f)))=f;
end
save_PS
