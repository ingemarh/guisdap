function dum=t2ps(site,rc)
if nargin<2, rc=0; end
if rc==0
 apustr='';
else
 apustr=['_' int2str(rc)];
end
t2psfile=['t_to_ps.txt' apustr '.' site];
t_to_ps=load(t2psfile,'-ascii');
nslic=1;
ch_adcint=[6];
ch_f=[14];
if site=='L'
 p_rep=1009920/2;
 ch_filter={'w83d60.fir'};
 nslic=10;
elseif rc>1
 p_rep=320000;
 ch_filter={'b125d60.fir'};
 nslic=25;
 ch_adcint=[4];
 ch_f=[12];
else
 p_rep=443904;
 ch_filter={'b83d90.fir'};
 if site=='T'
  nslic=9;
 end
end
d=find(t_to_ps(:,1)<p_rep);
td_t1=t_to_ps(d,1)';
td_t2=t_to_ps(d,2)';
td_am=t_to_ps(d,3)';
td_ch=t_to_ps(d,4)';
p_offsetppd=0;
for f=1:length(ch_f)
 td_ch(find(td_ch==ch_f(f)))=f;
end
if nslic>1
 slic=ones(size(td_t1))'*(0:nslic-1)*p_rep;
 td_t1=row(td_t1'*ones(1,nslic)+slic);
 td_t2=row(td_t2'*ones(1,nslic)+slic);
 td_am=row(td_am'*ones(1,nslic));
 td_ch=row(td_ch'*ones(1,nslic));
 p_rep=nslic*p_rep;
end
name_expr='arc1';
name_site=upper(site);
save_PS
