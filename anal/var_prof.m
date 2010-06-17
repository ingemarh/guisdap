% var_prof.m: Utility to get variances from the lag profiles
% GUISDAP v.8.2 03-05-27 Copyright EISCAT
%
function var_prof(N_averaged,M_averaged,N_avpref)
global d_var1 d_var2 d_data lpg_ra lpg_ri lpg_nt lpg_bcs ADDR_SHIFT ad_coeff
global a_code ad_code lpg_code lpg_lag lpg_ND

addr=1:min(length(ad_coeff),length(d_data));
coeff=ad_coeff(addr)'; coeff(find(ad_coeff(addr)==0))=1;
data=d_data(addr)./coeff;
dvar1=d_var1(addr)./coeff.^2;
dvar2=d_var2(addr)./coeff.^2;
N_avtot=N_avpref; % no points in profile to use
laver=length(N_averaged);
if M_averaged(2)==0
  N0=N_avtot;
else
  N0=ceil(N_avtot/M_averaged(2));
end
f0=ones(N0,1)/N0; L0=floor((N0-1)/2); R0=ceil((N0-1)/2);

lpgs=find(lpg_bcs=='s' | lpg_bcs=='x');
if ~isempty(a_code)
 lpgs=lpgs(find(ismember(lpg_code(lpgs),unique(a_code))));
end
lpgbad=[]; lpgok=[];
for lpg=lpgs
  addr=(0:lpg_nt(lpg)-1)'*lpg_ri(lpg)+lpg_ra(lpg)+ADDR_SHIFT;
  if laver==1
    Na=length(addr); N_av=Na*N_averaged;
    naver=N_averaged;
  else
    addr=addr(find(N_averaged(addr)));
    naver=N_averaged(addr);
    Na=length(addr); N_av=sum(naver);
  end
  if N_av<3
    lpgbad=[lpgbad lpg];
    d_var1(addr)=0;
    d_var2(addr)=0;
  else
    lpgok=[lpgok lpg];
    if Na<N0
      N=Na; f=ones(N,1)/N; L=floor((N-1)/2); R=ceil((N-1)/2);
      N_avtot=min(N_avtot,N_av);
    else
      f=f0; N=N0; L=L0; R=R0;
    end
    coef=conv(coeff(addr),f).^2;
    dat=conv(data(addr),f);
    if laver>1
      naver=conv(naver,f);
      coef=coef.*(M_averaged(1)./naver).^2; % All data normalised
    end
    var1=(conv(dvar1(addr),f)-dat.*dat./naver).*coef;
    var2=(conv(dvar2(addr),f)-dat.*conj(dat)./naver).*coef;
    L=ones(L,1); R=ones(R,1);
    d_var1(addr)=[L*var1(N);var1(N:Na);R*var1(Na)];
    d_var2(addr)=[L*var2(N);var2(N:Na);R*var2(Na)];
  end
end
if ~isempty(lpgbad)
  warning('GUISDAP:default','Making dirty variances, increase integration or set analysis_control(4)=2 for better estimates')
  [dum,i,j]=unique([lpg_code(lpgbad);lpg_lag(lpgbad)]','rows');
  for ind=i'
    lpgs=lpgok(find(lpg_code(lpgbad(ind))==lpg_code(lpgok) & lpg_lag(lpgbad(ind))==lpg_lag(lpgok)));
    addo=[];
    for lpgo=lpgs
      addo=[addo;(0:lpg_nt(lpgo)-1)'*lpg_ri(lpgo)+lpg_ra(lpgo)+ADDR_SHIFT];
    end
    if ~isempty(addo)
      lpgND=mean(lpg_ND(lpgs)); var1=mean(d_var1(addo)); var2=mean(d_var2(addo));
      for lpg=lpgbad(find(j==find(ind==i)))
        coef=lpgND/lpg_ND(lpg);
        addr=(0:lpg_nt(lpg)-1)'*lpg_ri(lpg)+lpg_ra(lpg)+ADDR_SHIFT;
        d_var1(addr)=var1*coef;
        d_var2(addr)=var2*coef;
      end
    end
  end
end

N_averaged=M_averaged(1); % Sats removed in cal
lpgs=find(lpg_bcs=='b' | lpg_bcs=='c');
if ~isempty(a_code)
 lpgs=lpgs(find(ismember(lpg_code(lpgs),unique(a_code))));
end
for lpg=lpgs
  addr=(0:lpg_nt(lpg)-1)*lpg_ri(lpg)+lpg_ra(lpg)+ADDR_SHIFT;
  Na=length(addr);
  if Na<N0
    N=Na; f=ones(N,1)/N; L=floor((N-1)/2); R=ceil((N-1)/2);
  else
    f=f0; N=N0; L=L0; R=R0;
  end
  if any(coeff(addr)-1)
    error('GUISDAP:default','Well, how did we get here...')
  end
  dat=conv(data(addr),f);
  var1=conv(dvar1(addr),f)-dat.*dat/N_averaged;
  var2=conv(dvar2(addr),f)-dat.*conj(dat)/N_averaged;
  d_var1(addr)=[ones(L,1)*var1(N);var1(N:Na);ones(R,1)*var1(Na)];
  d_var2(addr)=[ones(L,1)*var2(N);var2(N:Na);ones(R,1)*var2(Na)];
end

if N_avtot<N_avpref
  warning('GUISDAP:default','%.0f points may not be enough for reliable variance determination',N_avtot)
end
