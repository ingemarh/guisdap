% find_sampling.m: finds sampling times for all virtual channels
% GUISDAP v1.60   96-05-27 Copyright Asko Huuskonen, Markku Lehtinen
% 
% finds sampling times for all virtual channel and stores results
% to a global variable vc_sampling
% NOTE: This is a EISCAT specific function
%
% See also: init_EISCAT 
% function find_sampling
  function find_sampling
  
global vc_sampling vc_ch vc_t1 vc_t2 vc_envo vc_env
global td_ch td_t1 td_t2 td_am p_rep 
global name_site

s_ind=1;
vc_sampling=zeros(1000,4);

ind1=find(td_ch==0);  % indicates calibration times
cal_start=[td_t1(ind1)]; % AH 94-04-20 Changed here due to change in td_arrange
cal_stop=[td_t2(ind1)];
  
for vc=find(vc_ch>0) % These channels in use
  % find lines that belong to the virtual channel
  ind=find(td_ch==vc_ch(vc) & td_t1>=vc_t1(vc) & td_t2<=vc_t2(vc) & td_am==2);
  if length(ind)==0,
    fprintf(' Error in virtual channel specification?\n')
    fprintf(' Virtual channel %.0f contains no sampling intervals\n',vc)
    error(' ')
  end
  t1=td_t1(ind);
  t2=td_t2(ind);
  am=td_am(ind);

  % Look first for the signal sampling, it should be there. The logic is 
  % that the first sampling period after the transmission is the signal sampling
  ind1=find(t1>vc_envo(vc));
  if name_site=='R' % but not so for remote!
    ind1=ind1-1; if isempty(ind1), ind1=1; end
  end
  [dummy,ind]=min(t1(ind1));
  ind1=ind1(ind);
  vc_sampling(s_ind,:)=[vc,type2ind('s'),t1(ind1),t2(ind1)]; 
  s_ind=s_ind+1; 

  for i=1:length(t1); % Look if calibration sampling done
    if any(cal_start<=t1(i) & t2(i)<=cal_stop),
      vc_sampling(s_ind,:)=[vc,type2ind('c'),t1(i),t2(i)];
      s_ind=s_ind+1;
      ind1=[ind1, i];
    end
  end
  t1(ind1)=[]; 
  t2(ind1)=[]; 
  am(ind1)=[];
 
  len=length(t1); % The ones remaining must be background
  vc_sampling(s_ind+(0:len-1),:)=[vc*ones(len,1),type2ind('b')*ones(len,1),t1',t2'];
  s_ind=s_ind+len;
end

len=length(vc_sampling(:,1));
vc_sampling(s_ind:len,:)=[];
