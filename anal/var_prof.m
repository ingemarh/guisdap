% var_prof.m: Utility to get variances from the lag profiles
% GUISDAP v.8.2 03-05-27 Copyright EISCAT
%
function var_prof(N_averaged,N_avpref)
global d_var1 d_var2 d_data lpg_ra lpg_ri lpg_nt lpg_bcs ADDR_SHIFT ad_coeff
global a_code ad_code lpg_code

addr=1:min(length(ad_coeff),length(d_data));
coeff=ad_coeff(addr)'; coeff(find(ad_coeff(addr)==0))=1;
data=d_data(addr)./coeff;
dvar1=d_var1(addr)./coeff.^2+data.*data/N_averaged;
dvar2=d_var2(addr)./coeff.^2+data.*conj(data)/N_averaged;
N_avtot=N_avpref; % no points in profile to use
N0=ceil(N_avtot/N_averaged);
f0=ones(N0,1)/N0; L0=floor((N0-1)/2); R0=ceil((N0-1)/2);

lpgs=find(lpg_bcs=='s' | lpg_bcs=='x');
if ~isempty(a_code)
 lpgs=lpgs(find(ismember(lpg_code(lpgs),unique(a_code))));
end
for lpg=lpgs
  addr=(0:lpg_nt(lpg)-1)*lpg_ri(lpg)+lpg_ra(lpg)+ADDR_SHIFT;
  Na=length(addr); N_av=Na*N_averaged;
  if N_av<3
    d_var1(addr)=0;
    d_var2(addr)=0;
  else
    if Na<N0
      N=Na; f=ones(N,1)/N; L=floor((N-1)/2); R=ceil((N-1)/2);
      N_avtot=min(N_avtot,N_av);
    else
      f=f0; N=N0; L=L0; R=R0;
    end
    coef=conv(coeff(addr),f).^2;
    dat=conv(data(addr),f);
    var1=(conv(dvar1(addr),f)-dat.*dat/N_averaged).*coef;
    var2=(conv(dvar2(addr),f)-dat.*conj(dat)/N_averaged).*coef;
    d_var1(addr)=[ones(L,1)*var1(N);var1(N:Na);ones(R,1)*var1(Na)];
    d_var2(addr)=[ones(L,1)*var2(N);var2(N:Na);ones(R,1)*var2(Na)];
  end
end

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
    error(sprintf('Well, how did we get here...\n'))
  end
  dat=conv(data(addr),f);
  var1=conv(dvar1(addr),f)-dat.*dat/N_averaged;
  var2=conv(dvar2(addr),f)-dat.*conj(dat)/N_averaged;
  d_var1(addr)=[ones(L,1)*var1(N);var1(N:Na);ones(R,1)*var1(Na)];
  d_var2(addr)=[ones(L,1)*var2(N);var2(N:Na);ones(R,1)*var2(Na)];
end

if N_avtot<N_avpref
  fprintf('Warning: %.0f points may not be enough for reliable variance determination\n',N_avtot)
end
