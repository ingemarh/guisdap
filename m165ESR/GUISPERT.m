% GUISPERT.m: special hacks - makes some more space available
% GUISDAP v1.70   01-11-30 Copyright EISCAT, Huuskonen&Lehtinen
%
% script for GUISPERTs and other power users. Put here any commands by
% which you wish to change the data before the analysis is started
%
% See also: GUIZARD
%
d_var1=real(d_var1);

if strcmp(name_expr,'gup1')
  fprintf(' GUISPERT gup1 : scaling alternating codes\n')
  % The alternating code signal data has been scaled by 4
  lpgs=find((lpg_code>=5 & lpg_code<=8) & lpg_bcs=='s');
  addr=lpg_addr(lpgs);
  d_data(addr+1)=4*d_data(addr+1);
  d_var1(addr+1)=16*d_var1(addr+1);
  d_var2(addr+1)=16*d_var2(addr+1);
  % Combine the background and calibration data blocks for AC
  fprintf(' GUISPERT gup1 : combining alternating code calibrations\n')
  for code=5:8
    % Background
    lpgs=find(lpg_code==code & lpg_bcs=='b' & lpg_lag==0);
    addr1=lpg_addr(lpgs(1));
    addr2=lpg_addr(lpgs(2));
    d_data(addr1+1)=(d_data(addr1+1)+d_data(addr2+1))/2;
    d_var1(addr1+1)=(d_var1(addr1+1)+d_var1(addr2+1))/4;
    d_var2(addr1+1)=(d_var2(addr1+1)+d_var2(addr2+1))/4;
    % Calibration
    lpgs=find(lpg_code==code & lpg_bcs=='c' & lpg_lag==0);
    addr1=lpg_addr(lpgs(1));
    addr2=lpg_addr(lpgs(2));
    d_data(addr1+1)=(d_data(addr1+1)+d_data(addr2+1))/2;
    d_var1(addr1+1)=(d_var1(addr1+1)+d_var1(addr2+1))/4;
    d_var2(addr1+1)=(d_var2(addr1+1)+d_var2(addr2+1))/4;
  end
end

if strcmp(name_expr,'gup1') |  strcmp(name_expr,'gup0') | strcmp(name_expr,'gup00')
  if strcmp(name_expr,'gup1'), maxcode=4; else maxcode=8; end
  fprintf(' GUISPERT gup0/1 : combining long pulse calibrations\n')
  for code=1:maxcode
    lagvalues=diff_val(lpg_lag(find(lpg_code==code & lpg_bcs=='b')));
    for lags=lagvalues
      lpgs=find(lpg_code==code & lpg_bcs=='b' & lpg_lag==lags);
	  addr1=lpg_addr(lpgs(1));
	  addr2=lpg_addr(lpgs(2));
      d_data(addr1+1)=(d_data(addr1+1)+d_data(addr2+1))/2;
      d_var1(addr1+1)=(d_var1(addr1+1)+d_var1(addr2+1))/4;
      d_var2(addr1+1)=(d_var2(addr1+1)+d_var2(addr2+1))/4;
    end
    lpgs=find(lpg_code==code & lpg_bcs=='c' & lpg_lag==0);
    addr1=lpg_addr(lpgs(1));
    addr2=lpg_addr(lpgs(2));
    d_data(addr1+1)=(d_data(addr1+1)+d_data(addr2+1))/2;
    d_var1(addr1+1)=(d_var1(addr1+1)+d_var1(addr2+1))/4;
    d_var2(addr1+1)=(d_var2(addr1+1)+d_var2(addr2+1))/4;
  end
end
% d_var1 = real(d_var1);
if strcmp(name_expr,'cp4b') % 2-beam
  fprintf(' GUISPERT ',name_expr,': reorganising data for beam %d\n',vhf_beam)
  d_data = cp4bv2gup(d_data,vhf_beam);
  d_var1 = cp4bv2gup(d_var1,vhf_beam);
  d_var2 = cp4bv2gup(d_var2,vhf_beam);
elseif strcmp(name_expr,'tau1') % 2-beam
  fprintf(1,' GUISPERT for %s: reorganising data for beam %d\n',name_expr, vhf_beam)
  d_data = tau1v2gup(d_data,vhf_beam);
  d_var1 = tau1v2gup(d_var1,vhf_beam);
  d_var2 = tau1v2gup(d_var2,vhf_beam);
elseif strcmp(name_expr,'tau1_singel')
  fprintf(1,' GUISPERT for %s : reorganising data\n', name_expr)
  d_data = tau1_singel_v2gup(d_data);
  d_var1 = tau1_singel_v2gup(d_var1);
  d_var2 = tau1_singel_v2gup(d_var2);
end
