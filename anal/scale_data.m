% scale_data.m: data is scaled to Kelvins
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% Here the data points are divided by the correlator algorithm factors lpg_ND.
% By this procedure different signal/background/calibration algorithms are 
% handled automatically. The spectral ambiguity functions are scaled 
% accordingly by routine 'scale_lpgwom'
%
% Next the data is transformed to units of K with the calibration data
% This is not an optimal solution, and will be reconsidered in later releases
%
% Warning: NEVER perform this operation twice, as the resuls is returned
%          in the input variables 'd_data', 'd_var1' and 'd_var2'
% See also: an_start, scale_lpgwom
%
% modified to use scaler calibration temperature derived from the
% parameter block entry

function scale_data

global lpg_ra lpg_ND lpg_cal lpg_bac lpg_bcs d_data d_var1 d_var2 ...
  ADDR_SHIFT calTemp sysTemp a_code lpg_code

%*************** SCALING BY CORRELATOR ALGORITHM FACTORS *****************
data=d_data;
d_data=ones(size(data))*NaN;
lpgs=find(lpg_bcs~='g');% handle all but garbage groups
if ~isempty(a_code)
  lpgs=lpgs(find(ismember(lpg_code(lpgs),unique(a_code))));
end
for lpg=lpgs
  addr=lpg_addr(lpg);    % result memory addresses for lpg
  addr=addr+ADDR_SHIFT; % To change from radar to Matlab addressing
  d_data(addr)=data(addr)/lpg_ND(lpg);
  d_var1(addr)=d_var1(addr)/(lpg_ND(lpg)*lpg_ND(lpg));
  d_var2(addr)=d_var2(addr)/(lpg_ND(lpg)*lpg_ND(lpg));
end

%*********************** AND BY CALIBRATION POWER ************************
%calculate first scale for all calibration measurements
calibs=diff_val(lpg_cal(lpgs));      % find all different values
calibs=calibs(find(calibs>0)); % Accept non-zero values
scale=zeros(size(lpg_cal));
if ~isempty(calTemp)
 sysTemp=[];
 for cal=calibs
  bac=lpg_bac(cal);
  bac_power=median(d_data(lpg_addr(bac)+ADDR_SHIFT));
  cal_power=median(d_data(lpg_addr(cal)+ADDR_SHIFT));
  if length(calTemp)>1
    scale(cal)=(cal_power-bac_power)/calTemp(lpg_code(cal));
  else
    scale(cal)=(cal_power-bac_power)/calTemp;
  end
  sysTemp=[sysTemp;bac_power/scale(cal)];
 end
elseif ~isempty(sysTemp)
%*********************** OR BY BACKGROUND POWER ************************
 for cal=calibs
  bac=lpg_bac(cal);
  bac_power=median(d_data(lpg_addr(bac)+ADDR_SHIFT));
  scale(cal)=bac_power/sysTemp;
 end
else
 error('GUISDAP:default','No calibration temperature')
end
for lpg=lpgs
 cal=lpg_cal(lpg);
 addr=lpg_addr(lpg)+ADDR_SHIFT; % To change from radar to Matlab addressing
 d_data(addr)=d_data(addr)/scale(cal);
 d_var1(addr)=d_var1(addr)/(scale(cal)*scale(cal));
 d_var2(addr)=d_var2(addr)/(scale(cal)*scale(cal));
end
