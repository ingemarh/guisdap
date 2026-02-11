function dum=t2ps(site,rc)
if nargin<2, rc=0; end
if rc==0
 apustr='';
else
 apustr=['_' int2str(rc)];
end
p1=31800; p2=p1+10600; tp=320; r1=3*tp+40; r2=tp+80; r3=10407; r4=26685;
td_t1=[0 r1 r3 p1 p1+r2];
td_t2=[3*tp r3 r4 p1+tp p1+r3];
td_am=[1 2 2 1 2];
td_t1=[td_t1 p2+[0 2*tp r1 r3 p1 p1+r2]];
td_t2=[td_t2 p2+[2*tp 3*tp r3 r4 p1+tp p1+r3]];
td_am=[td_am [1 -1 2 2 1 2]];
td_t1=[td_t1 2*p2+[0 tp 2*tp r1 r3 p1 p1+r2]];
td_t2=[td_t2 2*p2+[tp 2*tp 3*tp r3 r4 p1+tp p1+r3]];
td_am=[td_am [1 -1 1 2 2 1 2]];
td_t1=[td_t1 3*p2+[0 tp r1 r3 p1 p1+r2]];
td_t2=[td_t2 3*p2+[tp 3*tp r3 r4 p1+tp p1+r3]];
td_am=[td_am [1 -1 2 2 1 2]];
td_ch=ones(size(td_am));
ch_f=430e6;
ch_adcint=[2.5];
ch_filter={'CIC_127_80e6_300e3_d40'};
p_rep=4*p2;
if site=='r'
 p_offsetppd=-3000;
elseif site=='3'
 p_offsetppd=0;
else
 error('giveup')
end
name_expr='sy3x320a';
name_site=upper(site);
save_PS
