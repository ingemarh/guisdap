% sample_times.m: returns the sampling times for a virtual channel.
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
% 
% Input parameters:
% vc_sampling (global): matrix of sampling start and end times
%                       Each row is [vc, type, starttime, endtime];
% vc    : virtual channel number
% stype : reception type in coded form
% Output parameters:
% start_sampling: Sampling start times for the virtual channel and reception type
%  stop_sampling: Sampling   end times for the virtual channel and reception type
% ind : Lines in the vc_sampling matrix, which contain the data 
% 
% See also: pulse_times, type2ind
% function [start_sampling, stop_sampling,ind]=sample_times(vc,stype)
  function [start_sampling, stop_sampling,ind]=sample_times(vc,stype)

global vc_sampling p_rep vc_t1 vc_ch

% For most cases, this line is what is sufficient
ind=find(vc_sampling(:,1)==vc & vc_sampling(:,2)==stype);

% Now certain virtual channels do not have background and calibration
% because they are measured at time defined for some other virtual channel
if length(ind)==0 & stype==type2ind('s');
  fprintf(' \nSerious case, signal sampling missing for virtual channel %.0f\n',vc)
  dbstop error, error(' Entering debug mode')
elseif  length(ind)==0 & (stype==type2ind('b') | stype==type2ind('c'));
  ind=[]; vct=[];
  for vcs=find(vc_ch==vc_ch(vc) & 1:length(vc_ch)~=vc); % these vc on the same channel
    ind1=find(vc_sampling(:,1)==vcs & vc_sampling(:,2)==stype)'; % Background/calibration
    ind=[ind ind1];
    vct=[vct vcs*ones(size(ind1))];
  end
  if length(ind)>0; % Background/calibration found
    starts=[vc_sampling(ind,3) vc_sampling(ind,3)+p_rep]; % To handle the last vc on channel
    ind=[ind ind];
    vct=[vct vct];
    ind1=find(starts>vc_t1(vc)); % find sampling times following the vc
    ind=ind(ind1(1)); % take the first one as the best guess    This may be wrong
%    fprintf(' \nTaking back/cal sampling times to virtual channel %.0f from channel %.0f\n',vc, vct(ind1))
  elseif length(ind)==0; % No badkground/calibration found
    fprintf(' \nSerious case, backgr/cal sampling missing for real channel %.0f\n',vc_ch(vc))
    dbstop error, error(' Entering debug mode')
  end
end

start_sampling=vc_sampling(ind,3);
stop_sampling=vc_sampling(ind,4);
