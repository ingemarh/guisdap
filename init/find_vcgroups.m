% find_vcgroups: a utility to find virtual channels with similar transmissions
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% Several virtual channels often have similar transmission envelopes and receiver
% impulse responses. It will make the program execution faster, if ambiguity
% function calculations are done only once for all these channels. This function
% checks the envelopes and impulse responses and form the virtual channel groups
%
% See also: init_GUP
%
fprintf('\n\nGrouping the virtual channels in virtual channel groups.\n')
vc_group=zeros(size(vc_ch),'int32');

group=1;
vcs=find(vc_ch~=0); % These channels are in use
while length(vcs)>0,
  vc=vcs(1);
  %index=find(max(abs(vc_env(:,vcs)-vc_env(:,vc)*ones(1,length(vcs))))<100*eps ...
  %       & max(abs(vc_p(:,vcs)-vc_p(:,vc)*ones(1,length(vcs))))<100*eps);
  index=find(all(vc_env(:,vcs)==(vc_env(:,vc)*ones(1,length(vcs)))) ...
         & all(vc_p(:,vcs)==(vc_p(:,vc)*ones(1,length(vcs)))));
  vc_group(vcs(index))=group*ones(size(index));
  fprintf(' Group %.0f contains virtual channels ',group);
  fprintf(' %.0f',vcs(index)); fprintf('\n')
  group=group+1;
  vcs(index)=[];
end
