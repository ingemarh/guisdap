% pulse_times.m: Find the times of the pulse leading edges. 
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% Phase transitions are not counted
% Parameters
% vc:         virtual channel number
% pulsetimes: times in p_dtau units
%
% See also: sample_times
%
% function pulsetimes=pulse_times(vc)
  function pulsetimes=pulse_times(vc)

global vc_env vc_envo

% find lines that belong to the virtual channel and contain transmission
%ind=find(td_ch==vc_ch(vc) & td_t1>=vc_t1(vc) & td_t2<=vc_t2(vc) & abs(td_am)==1);
%pulsetimes=td_t1(ind);

% Calculation based on stored envelopes. Note that the result is not the same
% for coded pulses. This version gives only the start time of the whole pulse,
% whereas the one above gives also all the phasechanges.
ind=find([0; vc_env(:,vc)]==0 & [vc_env(:,vc); 0]~=0);
pulsetimes=vc_envo(vc)+ind+1-abs(vc_env(ind,vc));
