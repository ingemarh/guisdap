% impuls_resp.m: Calculates filter responces from ch_filter variable.
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% Calculates the filter impulse responses, based on the data saved
% in the variable ch_filter by the PS_LIFILT and PS_BUFILT routines
%
% See also: lin_filt but_filt

ch_p=[];
if iscell(ch_filter)
 for ind=1:length(ch_filter)
  impulsen=get_impresp([path_expr char(ch_filter(ind))],p_dtau)';
  l1=size(ch_p,1); l2=length(impulsen);
  if ind==1 | l1>=l2
   ch_p(:,ind)=[impulsen;zeros(l1-l2,1)];
  elseif l1<l2
   ch_p((l1+1):l2,1:(ind-1))=zeros(l2-l1,ind-1);
   ch_p(:,ind)=impulsen;
  end
 end
 clear ind l1 l2 impulsen
else
channels=find(ch_filter(1,:)==1 | ch_filter(1,:)==2); % these channels in use
while length(channels)>0,
  filtertype=ch_filter(1,channels(1));
  BW=ch_filter(2,channels(1));
  ind=find(ch_filter(1,channels)==filtertype & ch_filter(2,channels)==BW);
  if filtertype==1,
    but_filt(BW,channels(ind));
  elseif  filtertype==2,
    lin_filt(BW,channels(ind));
  end
  channels(ind)=[];
end
clear filtertype BW ind channels
end
