% chk_par2.m: Utility to check analysis parameters and make small repairs xxx
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%

if exist('analysis_classic')
  a_classic=analysis_classic;
else
  a_classic=0;
end

fac=1000/(v_lightspeed/2)/(p_dtau*1e-6);
if exist('analysis_range')
  a_range=analysis_range*fac;
elseif exist('analysis_altit')
  a_range=height_to_range(analysis_altit,ch_el(1));
else
  a_range=[0 100000];
end
 
if exist('analysis_addr')
  a_addr=analysis_addr;
  a_adstart=analysis_adstart;
  a_adend=[a_adstart(2:length(a_adstart))-1,length(a_addr)];
  return
elseif a_classic
  minrange=min(a_range);
  maxrange=max(a_range);
  a_range=[];
  for code=diff_val(lpg_code)
    lags=lpg_lag(find(lpg_code==code & lpg_bcs=='s'));
    if max(lags)>0,  % ACF data, analyze these
      lpg=find(lpg_code==code & lpg_bcs=='s' & lpg_lag==max(lags));
      ranges=lpg_h(lpg)+(0:lpg_nt(lpg)-1)*lpg_dt(lpg);
      ranges=ranges(find(ranges>=minrange & ranges<=maxrange));
      lenr=length(ranges);
      if lenr>0
        len=length(a_range);
        a_range=[a_range ranges-lpg_dt(lpg)/2 max(ranges)+lpg_dt(lpg)/2 0];
        acode(code,len+(1:lenr))=ones(1,lenr);
        acode(code,len+lenr+1)=0;
      end
    end
    a_minwidth=zeros(1,length(a_range));
    a_maxwidth=1000000000*ones(1,length(a_range));
  end
else
 
  lenr=length(a_range);
  a_minwidth=zeros(1,lenr-1);
  a_maxwidth=inf*ones(1,lenr-1);

  if exist('analysis_minwidth')
    len=length(analysis_minwidth);
    if exist('analysis_altit')
      a_minwidth(1:len)=height_to_range(analysis_minwidth,ch_el(1));
    else
      a_minwidth(1:len)=analysis_minwidth*fac;
    end
  end 

  if exist('analysis_maxwidth')
    len=length(analysis_maxwidth);
    if exist('analysis_altit')
      a_maxwidth(1:len)=height_to_range(analysis_maxwidth,ch_el(1));
    else
      a_maxwidth(1:len)=analysis_maxwidth*fac;
    end
  end

  acode=[];
  gates=[];
  codes=[];
  if ~isempty(a_code)
    if length(a_code)==length(a_range)
      temp=a_code;
      while any(temp>0)
        code=rem(temp,10);
        temp=floor(temp/10);
        ind=find(code>0);
        gates=[gates,ind];
        codes=[codes,code(ind)];
      end
      for i=1:max(codes)
        ind=find(codes==i);
        len=length(ind);
        if len>0; acode(i,gates(ind))=ones(1,len); end
      end
      clear code temp ind gates codes i
    elseif max(a_code)>max(lpg_code)
      fprintf('Analysis_code variable not well defined and neglected\n')
      a_code=[];
    elseif length(name_ant)==3 & ~all(ismember(a_code,lpg_code))
      name_ant=[name_ant 96+a_code(1)];
    end
  end
end

% form the addresses for each gate 
a_addr=[];
a_adstart=[];
a_adend=[];
diffran=diff(a_range);  
for gate=find(diffran>0)
  addr=find(ad_range>a_range(gate) & ad_range<=a_range(gate+1));
  if ~isempty(addr)
    % Select suitable codes 
    if length(acode)>0
      ind1=[];
      for code=find(acode(:,gate)==1)';
        ind1=[ind1,find(ad_code(addr)==code)];
      end
      addr=addr(ind1);
    elseif ~isempty(a_code)
      addr=addr(find(ismember(ad_code(addr),a_code)));
    end
    % Remove too narrow or too broad responses 
    ind1=find(ad_w(addr)>a_minwidth(gate) & ad_w(addr)<a_maxwidth(gate) );
    addr=addr(ind1);
  end
  if a_control(4)==1
    % Remove addresses for which variance have not been determined
    if any(~real(d_var1(addr))) & isempty(a_addr)
      fprintf('Warning: Points removed from analysis due to bad variance determination\n')
    end
    addr=addr(find(real(d_var1(addr))));
  end

  if length(addr)>1, % store addresses if at least two points found
    len=length(a_addr);
    a_adstart=[a_adstart,len+1];
    a_addr=[a_addr,addr-ADDR_SHIFT]; % From matlab indexing to radar indexing
    a_adend=[a_adend,len+length(addr)];
  end
end

if isempty(a_addr) & isempty(d_saveintdir)
  fprintf(' No correlator result memory locations have been selected\n')
  fprintf(' Check the data selection parameters\n')
  fprintf(' Execution will be stopped\n')
  error(' ')
end

%clear fac minrange maxrange code codes lags lpg ranges 
%clear lenr len diffran gate gates addr ind1 ind i temp
