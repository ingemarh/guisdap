function dum = t2ps(site, rc)

apustr = ['_', num2str(rc)];
t2psfile = ['t_to_ps.txt', '_2', '.', lower(site)];
t_to_ps = load(t2psfile,'-ascii');

p_offsetppd = 0; 
p_rep = 32 * 12800;
ch_adcint = [12];
ch_f = [440];
ch_filter = {'CIC_127_80e6_45e3_d200'};

td_t1 = t_to_ps(:,1)';
td_t2 = t_to_ps(:,2)';
td_am = t_to_ps(:,3)';
td_ch = t_to_ps(:,4)';
for f = 1:length(ch_f)
    d = find(td_ch == ch_f(f));
    td_ch(d) =f;
end
name_expr='syisrac16';
name_site=upper(site);
save_PS
