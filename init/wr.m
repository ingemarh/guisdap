% wr.m: function to calculate range ambiguity function
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% Parameters: 
% vch:    virtual channel number
% t1,t2 : sample times of first and second factor in the product
% dumdum: with four arguments calculates support for two-dimensional ambiguity
%         functions. There are cases where reduced ambiguity function is null
%         but the two-dimensional is not.
%
% See also: wrlpg
%
% function [wwr,r]=wr(vch,t1,t2,dumdum);
function [wwr,r]=wr(vch,t1,t2,dumdum);

global vc_penvabs vc_penv vc_penvo 
if nargin<4, dumdum=0; end

rt=wnz(vc_penvabs,vch);
rt=[min(rt)-1;rt;max(rt)+1];
len=length(rt); lenn=len-(t2-t1);
s=1;
st=rt(1)-1;  % Origin for the part of penvabs used
if st+t2-t1<0
  st=t1-t2;
end
if dumdum>1
 s=fix(min([t1 t2])/dumdum)*dumdum+1; lenn=t1-vc_penvo(vch)-st; len=lenn+(t2-t1);
end
%if abs(len-round(len))<eps('single'), len=round(len); end
%if abs(lenn-round(lenn))<eps('single'), lenn=round(lenn); end
if st+lenn>size(vc_penv,1)
  vc_penv=[vc_penv;zeros(st+lenn-size(vc_penv,1),size(vc_penv,2))];
end
if st+len>size(vc_penv,1)
  vc_penv=[vc_penv;zeros(st+len-size(vc_penv,1),size(vc_penv,2))];
end
%if abs(st-round(st))<eps('single'), st=round(st); end
stt=s+t2-t1;
%if abs(stt-round(stt))<eps('single'), stt=round(stt); end
wwr=vc_penv(st+(s:lenn),vch).*vc_penv(st+(stt:len),vch);
wwr=flipud(wwr);
r=t1-vc_penvo(vch)-st+(-lenn+1:1-s);
if dumdum==1, % calculate support for two-dim. ambiguities
  if st+lenn>size(vc_penvabs,1)
    vc_penvabs=[vc_penvabs;zeros(st+lenn-size(vc_penvabs,1),size(vc_penvabs,2))];
  end
  if st+len>size(vc_penvabs,1)
    vc_penvabs=[vc_penvabs;zeros(st+len-size(vc_penvabs,1),size(vc_penvabs,2))];
  end
  wwra=vc_penvabs(st+(1:lenn),vch).*vc_penvabs(st+(1+t2-t1:len),vch);
  wwra=flipud(wwra);
  suppind=find(wwra~=0);
  wwr=wwr(suppind); r=r(suppind);
end;
