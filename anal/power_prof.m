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
%
% See also: get_apriori
%
% function [PPprof,PPrange,PPsigma]=power_prof(addr,Debyecorr)
function [PPprof,PPrange,PPsigma]=power_prof(addr,Debyecorr)
  
global ad_range ch_el d_data lpg_womscaled ad_lpg p_om ad_coeff 
global v_epsilon0 v_Boltzmann v_elemcharge k_radar p_N0 ADDR_SHIFT
 
addr=addr+ADDR_SHIFT; % To change from radar to Matlab addressing
addr=addr(find(ad_coeff(addr)>0));
PPrange=ad_range(addr);
PPheight=range_to_height(PPrange,ch_el(1));
apriori=ionomodel(PPheight);

signal_power=real(d_data(addr));
len_eff=max(real(lpg_womscaled(ad_lpg(addr),:))')';
sigma=p_N0*signal_power./(ad_coeff(addr)'.*len_eff);

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
PPsigma=2*sigma/p_N0;

