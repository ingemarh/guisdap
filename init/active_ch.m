% active_ch.m: Tells which channels are transmitting and receiving at given time instants
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% Parameters
% t: time instants to check 
% See also: XMIT_pulse COR_status env
% function [XMITch,RECch,CALon]=active_ch(t);
  function [XMITch,RECch,CALon]=active_ch(t);
		
global vc_env vc_envo vc_sampling vc_ch

t=t(:);
tmin=min(t);
tmax=max(t);

[M,N]=size(vc_env);
XMITch=[];
if M
  for vc=find(tmin-vc_envo<=M & tmax-vc_envo>=0);
	  	if any(env(vc,t))
		    XMITch=vc_ch(vc);
			  	break
		  end
  end
end

RECch=[];CALon=0;
if length(vc_sampling)
  ind=find((tmin<=vc_sampling(:,4) & tmax>=vc_sampling(:,3)));
  if length(ind)
    RECch=diff_val(vc_ch(vc_sampling(ind,1)));
		  CALon=any(vc_sampling(ind,2)==type2ind('c'));
  end	
end

% Find which channel is transmitting at any of the times t
%[M,N]=size(chXR);
%if M<max(t); t=t(find(t<M)); end
%if length(t);
%  apu=full(chXR(t,:)).*(ones(length(t),1)*(1:N));apu=diff_val(apu(:));ch=apu(find(apu))';
%else,ch=[]; end
