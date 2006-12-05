% power_prof.m: calculates approximate electron densities
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
% 
% Function calculates the electron density from a given set of measurements
% using the temperature ratio values in the model ionosphere
% Note that this calculation operates in physical units instead of scaled units
% Input parameters:
% addr: addresses to use
% Debyecorr: Two different options are available
%          The first one is faster but neglects Debye correction (Debyecorr=0)
%          The other one is slower but includes Debye correction (Debyecorr=1)
% output: pp_prof : Raw electron density with the a priori Te/Ti model
%         pp_range: The range to the center of gate
%         pp_sigma: Raw power profile with Te/Ti=1, excl. Debye term
%         pp_std:   Standard error raw power
%         pp_w:     Range resolution
%
% See also: get_apriori
%
% function [PPprof,PPrange,PPsigma,PPstd,PPw]=power_prof(addr,Debyecorr)
function [PPprof,PPrange,PPsigma,PPstd,PPw]=power_prof(addr,Debyecorr)
  
global ad_range ch_el d_data lpg_womscaled ad_lpg p_om ad_coeff ad_lag ad_code ad_w
global v_epsilon0 v_Boltzmann v_elemcharge k_radar p_N0 d_var1 d_var2
 
dvar=real(d_var1+d_var2);
%addr=addr+ADDR_SHIFT; % To change from radar to Matlab addressing
addr=addr(find(ad_coeff(addr)>0 & dvar(addr)'>0));
PPrange=ad_range(addr);
PPw=ad_w(addr);
signal_power=real(d_data(addr));
d=find(ad_lag(addr));
if ~isempty(d)
  signal_power(d)=abs(d_data(addr(d)));
  dvar(addr(d))=2*d_var2(addr(d));
end
len_eff=max(real(lpg_womscaled(ad_lpg(addr),:))')';
sigma=p_N0*signal_power./(ad_coeff(addr)'.*len_eff);
sigma_std=sigma.*sqrt(dvar(addr))./signal_power;
if any(ad_lag(addr)>0)
  %Reduce no powerpoints but keep different codes apart
  rres=1; %range resolution given by p_dtau
  a3=[];
  for code=unique(ad_code(addr))'
    a1=find(code==ad_code(addr));
    a1_w=PPw(a1);
    ranges=round(ad_range(addr(a1))/rres)+1e-2*round((a1_w-mean(a1_w))/std(a1_w));
    for range=unique(ranges)
      a2=find(ranges==range);
      if length(a2)>1
        w2=1./sigma_std(a2).^2;
        sigma(a2(1))=sum(sigma(a2).*w2)/sum(w2);
	sigma_std(a2(1))=1/sqrt(sum(w2));
        a3=[a3 a2(2:end)];
      end
    end
  end
  sigma(a3)=[];
  sigma_std(a3)=[];
  PPrange(a3)=[];
  PPw(a3)=[];
end

PPheight=range_to_height(PPrange,ch_el(1));
PPsigma=2*sigma/p_N0;
PPstd=2*sigma_std/p_N0;
apriori=ionomodel(PPheight);
% Here one can choose of two different solutions
% The first one is faster but neglects Debye correction (Debyecorr=0)
% The other one is slower but includes Debye correction (Debyecorr=1)
% Note that this calculation operates in physical units instead of scaled
if Debyecorr,
  % solve the third order equation for electron density 
  ratio=apriori(:,3);
  Te=apriori(:,2).*ratio;
  ch=1;  % hyi hyi
  A=(k_radar(ch))^2*v_epsilon0*v_Boltzmann*Te/(v_elemcharge)^2;
  B=-sigma.*(1+ratio);
  C=-sigma.*A.*(2+ratio);
  D=-sigma.*A.*A;
  for i=1:length(A);
    apu=roots([1,B(i),C(i),D(i)]);
    % choose the root closest to the first order solution equal to -B.
    [hups,ind]=min(abs(apu+B(i)));
    res(i,1)=apu(ind);
  end
else
  % Solve only the first order equation (neglect Debye effect)
  ratio=apriori(:,3);
  res=sigma.*(1+ratio);
end

PPprof=res/p_N0;
