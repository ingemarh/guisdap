% get_apriori: finds apriori values from model ionospheres
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
% 
% This function find the center altitudes of the analysis gates and calls
% ionomodel to get the model ionosphere for these altitudes.
% Next, all zero lag measurements are used to get an initial estimate for
% the electron density
%
% See also: ionomodel, power_prof range_to_height
% Corrected 29 Sep 96 and 11 Dec 96 /AH
function get_apriori(simul)
 
global a_addr a_adstart a_adend a_range a_priori a_priorierror ADDR_SHIFT
global lpg_bcs lpg_lag ch_el p_N0 ad_w  ad_range di_results di_figures
global pp_profile pp_range pp_sigma pp_height a_chap

if nargin==0, simul=0; end

for gate=1:length(a_adstart)
  range(gate)=mean(ad_range(a_addr(a_adstart(gate):a_adend(gate))+ADDR_SHIFT));
end
height=range_to_height(range,ch_el(1));

% Exit here, if the a_priori model is loaded for simulation purposes,
% or it should not be updated with the measured raw electron denstity 
if simul
 [a_priori,a_priorierror]=ionomodel(height');
 a_priori=real_to_scaled(a_priori);
 a_priorierror=real_to_scaled(a_priorierror);
 return
end
% Calculate electron density estimates from zero lags, if available.
ind=find(lpg_bcs=='s' & lpg_lag==0);
if length(ind)>0,
  [pp_profile,pp_range,pp_sigma]=power_prof(lpg_addr(ind)',0);
  pp_height=range_to_height(pp_range,ch_el(1));

  if  di_figures(2),
    figure(di_figures(2)); clf
    p=plot(pp_profile*(p_N0/1e11),pp_height,'ro');
    a=get(gca,'xlim'); a=min([max([a;-0.5 -0.5]);100 100]);
    set(gca,'xlim',a,'NextPlot','replace'), set(p,'MarkerSize',2)
    title('Ne with model Te/Ti')
    ylabel('Altitude/km'), xlabel('Raw electron density/1e11'), drawnow
  end
end
% form the a priori model for the analysis
[a_priori,a_priorierror]=ionomodel(height');
% change from physical to scaled variables
a_priori=real_to_scaled(a_priori);
a_priorierror=real_to_scaled(a_priorierror);
if length(ind)>0 & isempty(a_chap)
 % Update the a priori electron density
 for gate=1:length(a_adstart)
  ind=find(abs(pp_range-range(gate))<ad_w(ADDR_SHIFT+a_addr(a_adstart(gate))));
  if length(ind)>0 & any(pp_profile(ind)>0),
   a_priori(gate,1)=mean(min(2e12/p_N0,max(5e9/p_N0,pp_profile(ind)))); 
   a_priorierror(gate,1)=100*a_priori(gate,1);
  end
 end
end
