% GUISPERT.m: special hacks - makes some more space available
% GUISDAP v1.70   01-11-30 Copyright EISCAT, Huuskonen&Lehtinen
%
% script for GUISPERTs and other power users. Put here any commands by
% which you wish to change the data before the analysis is started
%
% See also: GUIZARD
%
d_var1=real(d_var1);

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

  maxcode=4;
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
