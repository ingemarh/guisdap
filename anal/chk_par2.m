% chk_par2.m: Utility to check analysis parameters and make small repairs xxx
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%

if exist('analysis_classic')==1,
  a_classic=analysis_classic;
else
  a_classic=0;
end

fac=1000/(v_lightspeed/2)/(p_dtau*1e-6);
if exist('analysis_range')==1,
  a_range=analysis_range*fac;
elseif exist('analysis_altit')==1,
  a_range=height_to_range(analysis_altit,ch_el(1));
else
  a_range=[0 100000]; 
end
 
if exist('analysis_addr')==1,
  a_addr=analysis_addr;
  a_adstart=analysis_adstart;
  a_adend=[a_adstart(2:length(a_adstart))-1,length(a_addr)];
  return
elseif a_classic
  minrange=min(a_range);
  maxrange=max(a_range);
  a_range=[];
  for code=diff_val(lpg_code),
    lags=lpg_lag(find(lpg_code==code & lpg_bcs=='s'));
    if max(lags)>0,  % ACF data, analyze these
      lpg=find(lpg_code==code & lpg_bcs=='s' & lpg_lag==max(lags));
      ranges=lpg_h(lpg)+(0:lpg_nt(lpg)-1)*lpg_dt(lpg);
      ranges=ranges(find(ranges>=minrange & ranges<=maxrange));
      lenr=length(ranges);
      if lenr>0,
        len=length(a_range);
        a_range=[a_range ranges-lpg_dt(lpg)/2 max(ranges)+lpg_dt(lpg)/2 0];
        a_code(code,len+(1:lenr))=ones(1,lenr);
        a_code(code,len+lenr+1)=0;
      end
    end
    a_minwidth=zeros(1,length(a_range));
    a_maxwidth=1000000000*ones(1,length(a_range));
  end
else  
 
  lenr=length(a_range);
  a_minwidth=zeros(1,lenr-1);
  a_maxwidth=inf*ones(1,lenr-1); 

  if exist('analysis_minwidth')==1, 
    len=length(analysis_minwidth);
    if exist('analysis_altit')==1,
      a_minwidth(1:len)=height_to_range(analysis_minwidth,ch_el(1));
    else
      a_minwidth(1:len)=analysis_minwidth*fac;
    end
  end 

  if exist('analysis_maxwidth')==1, 
    len=length(analysis_maxwidth);
    if exist('analysis_altit')==1,
      a_maxwidth(1:len)=height_to_range(analysis_maxwidth,ch_el(1));
    else
      a_maxwidth(1:len)=analysis_maxwidth*fac;
    end
  end

  a_code=[];
  gates=[];
  codes=[];
  if exist('analysis_code')==1,
    if length(analysis_code)<length(a_range)-1;
      fprintf(' Analysis_code variable is too short and neglected\n')
    else
      temp=analysis_code;
      while any(temp>0);
        code=rem(temp,10);
        temp=floor(temp/10);
        ind=find(code>0);
        gates=[gates,ind];
        codes=[codes,code(ind)];
      end
      for i=1:max(codes)
        ind=find(codes==i);
        len=length(ind);
        if len>0; a_code(i,gates(ind))=ones(1,len); end
      end
      clear code temp ind gates codes i
    end
  end
end

% form the addresses for each gate 
a_addr=[];
a_adstart=[];
a_adend=[];
diffran=diff(a_range);  
if a_control(4)==1 & exist('N_averaged') & N_averaged<5
  dvar1=d_var1+d_data.*d_data/N_averaged;
  dvar2=d_var2+d_data.*conj(d_data)/N_averaged;
  N_avtot=Inf;
end
for gate=find(diffran>0),
  addr=find(ad_range>a_range(gate) & ad_range<=a_range(gate+1));
  if ~isempty(addr)
    % Select suitable codes 
    if length(a_code)>0,
      ind1=[];
      for code=find(a_code(:,gate)==1)';
        ind1=[ind1,find(ad_code(addr)==code)];
      end
      addr=addr(ind1);
    end
    % Remove too narrow or too broad responses 
    ind1=find(ad_w(addr)>a_minwidth(gate) & ad_w(addr)<a_maxwidth(gate) );
    addr=addr(ind1);
  end
  if length(addr)>1 & exist('dvar1')
    % Last chance to get the variances
    % Calculate from the profiles: Want at least 6 (accepts 3) points in each
    ij=ad_lpg(addr); lpg=unique(ij); naddr=[];
    for i=lpg
      n=addr(find(i==ij)); ln=length(n)*N_averaged; nn=n; ii=lpg_ri(i); in=0;
      while ln<5 & ln~=in
        in=ln;
        if n(1)-ADDR_SHIFT>lpg_ra(i)
          n=[n(1)-ii n]; ln=ln+N_averaged;
        end
        if (n(end)-ADDR_SHIFT-lpg_ra(i))/ii+1<lpg_nt(i)
          n=[n n(end)+ii]; ln=ln+N_averaged;
        end
      end
      if ln>2
        N_avtot=min(N_avtot,ln);
        ii=ad_coeff(n)'; in=ii/mean(ii); sii=mean(d_data(n)./in);
        d_var1(nn)=mean(dvar1(n)./in.^2)-sii.*sii/N_averaged;
        d_var2(nn)=mean(dvar2(n)./in.^2)-sii.*conj(sii)/N_averaged;
        naddr=[naddr nn];
      end
    end
    addr=naddr;
  end

  if length(addr)>1, % store addresses if at least two points found
    len=length(a_addr);
    a_adstart=[a_adstart,len+1];
    a_addr=[a_addr,addr-ADDR_SHIFT]; % From matlab indexing to radar indexing
    a_adend=[a_adend,len+length(addr)];
  end
end
if exist('dvar1')
  if N_avtot<5
    fprintf('Warning: %.0f points may not be enough for reliable variance determination\n',N_avtot)
  end
  clear lpg ii ij i naddr sii in n ln nn dvar1 dvar2 N_avtot
end

if length(a_addr)==0
  fprintf(' No correlator result memory locations have been selected\n')
  fprintf(' Check the data selection parameters\n')
  fprintf(' Execution will be stopped\n')
  error(' ')
end

%clear fac minrange maxrange code codes lags lpg ranges 
%clear lenr len diffran gate gates addr ind1 ind i temp
