function dummy=t2ps(name_site,version)

expname='mandala';
if name_site=='V'
    if version==4
        codefile='code_manda_v.txt'; % Name of file containing the code
        rep=1500; % Length of one subcycle in µs
        tb=2.4; % Baud length
        filtername={'b300d18.fir'}; % Fir filter
        sampl=1.2; % The sample rate
        freq=5; % The transmit frequency
        lowtail=59; % Size of lower tail
        hightail=59; % Size of upper tail
        t0=73; % Start of trasnmission, µs
        cal0=1473.4; % Start of calibration signal, µs
        cal1=1497; % Stop of calibration signal, µs
        s0=343; % Start of signal sampling, µs
        s1=1473.4; % Stop of signal sampling, µs
        c0=1485; % Start of calibration sampling, µs
        c1=1497; % End of calibration sampling, µs
        nl=25; % Number of times running through the code in on data dump
    end
else
    return
end
ac=textread(codefile,'%s');
[nsc,nb]=size(char(ac));
frac=tb/sampl;
t=t0+[0,tb*nb];
cal=[cal0,cal1];
s=[s0,s1];
c=[c0,c1];
ns=round((s1-s0)/sampl);
nc=round((c1-c0)/sampl);

par0 = [24;
    240;
    ns/frac-nb+1+lowtail+hightail;
    0;
    nsc;
    nsc*nl;
    ns;
    nb;
    frac;
    1;
    0;
    lowtail;
    hightail;
    0;
    0;
    1;
    20;
    0;
    0;
    0;
    0;
    1;
    150;
    0;];
par1=[par0;col((44-char(ac))')];
fid=fopen(append(expname,'_',lower(name_site),'a.par'),'w'); fprintf(fid,'%d\n',par1); fclose(fid);
par2=[par0;col((44-char(ac([end,1:end-1])))')];
fid=fopen(append(expname,'_',lower(name_site),'b.par'),'w'); fprintf(fid,'%d\n',par2); fclose(fid);

fprintf('ns=%d nc=%d\n',ns,nc)

ch_filter=filtername;
ch_adcint=sampl;
ch_f=freq;

p_rep=rep*nsc;
p_offsetppd=0;

ac=44-char(ac);
j=1;

for i=1:nsc
    % Tx
    i1=i-1;
    p=ac(i,1);
    tt=i1*rep;
    td_t1(j)=t(1)+tt;
    td_am(j)=p;
    td_ch(j)=freq;
    for k=2:nb
        if ac(i,k)~=p
            td_t2(j)=t(1)+tt+(k-1)*tb;
            p=ac(i,k);
            j=j+1;
            td_t1(j)=td_t2(j-1);
            td_am(j)=p;
            td_ch(j)=freq;
        end
    end
    td_t2(j)=t(2)+tt;
    j=j+1;

    % Signal
    td_t1(j)=s(1)+tt;
    td_am(j)=2;
    td_ch(j)=freq;
    td_t2(j)=s(2)+tt;
    j=j+1;

    % Cal/Bg
    if rem(i,2)
        td_t1(j)=cal(1)+tt;
        td_am(j)=1;
        td_ch(j)=0;
        td_t2(j)=cal(2)+tt;
        j=j+1;
    end
    td_t1(j)=c(1)+tt;
    td_am(j)=2;
    td_ch(j)=freq;
    td_t2(j)=c(2)+tt;
    j=j+1;
end

for f=1:length(ch_f)
    td_ch(find(td_ch==ch_f(f)))=f;
end

eval(append('save ',expname,upper(name_site),'_',int2str(version),'pat_PS ch_* p_* td_*'))