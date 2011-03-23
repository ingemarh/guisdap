% get_apriori: finds apriori values from model ionospheres
% GUISDAP v.8.2 03-08-27 Copyright EISCAT, Huuskonen&Lehtinen
% 
% This function find the center altitudes of the analysis gates and calls
% ionomodel to get the model ionosphere for these altitudes.
% Next, all zero lag measurements are used to get an initial estimate for
% the electron density
%
% See also: ionomodel, power_prof range_to_height
function get_apriori(simul)
 
global a_addr a_adstart a_adend a_range a_priori a_priorierror a_ppshortlags...
 a_code ADDR_SHIFT lpg_bcs ch_el ad_w ad_range ad_lpg ad_lag ad_code...
 di_figures p_N0 k_radar pp_profile pp_range pp_sigma pp_height pp_err pp_w

if nargin==0, simul=0; end

for gate=1:length(a_adstart)
  range(gate)=mean(ad_range(a_addr(a_adstart(gate):a_adend(gate))+ADDR_SHIFT));
end
ch=1; %hui hui
height=range_to_height(range,ch_el(ch));

% Exit here, if the a_priori model is loaded for simulation purposes,
% or it should not be updated with the measured raw electron denstity 
if simul
 [a_priori,a_priorierror]=ionomodel(height',0);
 a_priori=real_to_scaled(a_priori);
 a_priorierror=real_to_scaled(a_priorierror);
 return
end
% Calculate electron density estimates from low lags, if available.
addr=lpg_addr(find(lpg_bcs=='s'))+ADDR_SHIFT;
if ~isempty(a_code)
  addr=addr(find(ismember(ad_code(addr),unique(a_code))));
end
if a_ppshortlags(1)
  ad_h=range_to_height(ad_range(addr),ch_el(ch));
  %exp(1-ad_h/150)*a_ppshortlags*2e3/k_radar(ch);
  addr(find(ad_lag(addr)>3e7*a_ppshortlags(1)/k_radar(ch)./ad_h.^2))=[];
  addr(find(ad_lag(addr)>0 & ad_w(addr)>a_ppshortlags(2)/.15))=[];
  % default about 75(10) us at 100(300) km for UHF
else
  addr(find(ad_lag(addr)>0))=[];
end

ne_from_pp=NaN*ones(size(height'));
if length(addr)>0
  [pp_profile,pp_range,pp_sigma,pp_err,pp_w]=power_prof(addr',0);
  pp_height=range_to_height(pp_range,ch_el(ch));

  for gate=1:length(a_adstart)
    ind=find(abs(pp_range-range(gate))<ad_w(ADDR_SHIFT+a_addr(a_adstart(gate))));
    if length(ind)>0 & any(pp_profile(ind)>0)
      ne_from_pp(gate)=mean(p_N0*pp_profile(ind)); 
    end
  end

  if di_figures(2)
    drawnow, figure(di_figures(2))
    p=plot(pp_profile*p_N0/1e11,pp_height,'ro');
    a=get(gca,'xlim'); a=min([max([a;-0.5 -0.5]);100 100]);
    set(gca,'xlim',a,'NextPlot','replace'), set(p,'MarkerSize',2)
    title('Ne with model Te/Ti')
    ylabel('Altitude/km'), xlabel('Raw electron density/1e11'), drawnow
  end
end

% form the a priori model for the analysis
[a_priori,a_priorierror]=ionomodel(height',ne_from_pp);
% change from physical to scaled variables
a_priori=real_to_scaled(a_priori);
a_priorierror=real_to_scaled(a_priorierror);
