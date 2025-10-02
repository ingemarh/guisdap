% td_arrange.m:  checks the user defined variables and the td_-variables
% GUISDAP v1.60   96-05-27 Copyright Asko Huuskonen, Markku Lehtinen
% 
% This script checks the user defined variables and the td_-variables and
% corrects them in order to make programming of other functions/scripts easier
%
% Warning: This is NOT a user callable routine
%
% See also: vc_arrange

% Produce variable vc_mf if it was not defined by the user
if ~exist('vc_mf'), vc_mf=zeros(1,length(vc_t1)); end
if length(vc_mf)==0, vc_mf=zeros(1,length(vc_t1)); end

% Scale all time variables by the basic time unit
td_t1=gupround(td_t1/p_dtau); td_t2=gupround(td_t2/p_dtau);
vc_t1=gupround(vc_t1/p_dtau); vc_t2=gupround(vc_t2/p_dtau);
p_rep=gupround(p_rep/p_dtau);

% Produce another set of td-variables by adding p_rep to all  
% This increases flexibility in the virtual channel definition AH 94-04-20
%if any(vc_t1>p_rep) | any(vc_t2>p_rep)
 td_ch=int32([td_ch,td_ch]);
 td_am=[td_am,td_am];
 td_t1=[td_t1,td_t1+p_rep];
 td_t2=[td_t2,td_t2+p_rep];
%end

% Produce ADC intervals for all virtual channels in units of p_dtau
vc_adcint=zeros(1,length(vc_ch));
for vc=find(vc_ch>0)   % These virtual channels are in use
  vc_adcint(vc)=gupround(ch_adcint(vc_ch(vc))/p_dtau);
end

ind=find(vc_t1<0);
if length(ind)>0,vc_t1(ind)=vc_t1(ind)+p_rep;end
for vc=find(vc_ch>0)   % These virtual channels are in use
  % Find the lines belonging to the virtual channel 
  if vc_t1(vc)<vc_t2(vc),  
    ind=find(td_ch==vc_ch(vc) & td_t1>=vc_t1(vc) & td_t2<=vc_t2(vc) );
  else   % This is needed when the virtual channel extends to the next rep
    ind=find(td_ch==vc_ch(vc) & (td_t1>=vc_t1(vc) | td_t2<=vc_t2(vc)) );
    % Add p_rep to those td_t1 and td_t2 and vc_t2, which are smaller than vc_t1.
    ind1=find(td_t1(ind)<vc_t1(vc));
    ind2=find(td_t2(ind)<vc_t1(vc));
    if all(ind1==ind2),
      td_t1(ind(ind1))=td_t1(ind(ind1))+p_rep;
      td_t2(ind(ind1))=td_t2(ind(ind1))+p_rep;
      vc_t2(vc)=vc_t2(vc)+p_rep;
    else
      error('Error in td_arrange, something funny with td_t1 and td_t2')
    end
  end

end
clear vc ind ind1
