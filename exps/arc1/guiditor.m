%if analysis_ppcombine
% if ~exist('pp_sigma','var')
%  global pp_range pp_sigma pp_err pp_w
% end
% [pp_range,ia,ic]=unique(pp_range); 
% npp=length(pp_range);
% pp_w=pp_w(ia);
% PPerr=zeros(npp,1); PPsigma=PPerr;
%% for i=1:length(pp_range)
%  ind=find(ic==i);
%  Pvar=1./pp_err(ind).^2;
%  PPerr(i)=1/sqrt(sum(Pvar));
%  PPsigma(i)=sum(pp_sigma(ind).*Pvar)/sum(Pvar);
% end
% pp_err=PPerr; pp_sigma=PPsigma;
%end
