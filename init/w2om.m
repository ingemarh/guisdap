% w2om.m: calculation of 2-dim ambiguity functions
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% calculates the two-dimension range-lag and range-omega ambiguity functions
% for virtual channel 'vc' and sampling times 't1' and 't2'
% Output:
% ww  : two-dimensional range-omega ambiguity function
% wS  : range axis (common to ww and ww2)
% om  : frequency axis
% ww2 : two-dimensional range-lag ambiguity function
% wL  : lag axis
%
% See also wr plot_w2
%
% function [ww,wS,om,ww2,wL]=w2om(vc,t1,t2)
  function [ww,wS,om,ww2,wL]=w2om(vc,t1,t2)
%

  global vc_p vc_env vc_penvo vc_ch p_om p_om0 p_dtau
  w_om0=0; 
  pin=wnz(vc_p,vc);p=vc_p(pin,vc);lp=length(pin);
  p0=min(pin); p1=max(pin);
  env=[zeros(lp-1,1);vc_env(:,vc);zeros(lp-1,1)];

% range support of the full two-dimensional ambiguity function
% important for multipulses

  [w,rs]=wr(vc,t1,t2,1);rs=row(rs);  
                                  
% two-dimensional ambiguity lag supports from the filter 
% response and pulse envelopes 
  Ax=(p1:-1:p0);
  fp=flipud(p);
  x0=t1-t2;i=sqrt(-1);                    
  len=length(p_om); om=row(p_om(((len+1)/2):len))*p_om0(vc_ch(vc))*p_dtau*1e-6;
  dom=om(2)-om(1);Nsave=length(om);
  phaseshift=exp(-i*x0*om);                     % lag=x0

  irow=0;
  lena=length(fp);
  kit=(-dom*(0:(lena-1)))'*(0:(Nsave-1));
  st=sin(kit); ct=cos(kit);
  for S=rs
    irow=irow+1;
    ind=-Ax-S+lp;
    ampambg1=fp.*env(t1-vc_penvo(vc)+ind); 
    ampambg2=fp.*env(t2-vc_penvo(vc)+ind);   
%   term1a=row(fft(ampambg1,Nfft));term2a=row(fft(ampambg2,Nfft));
%   ww1a(irow,:)=phaseshift.*term1a(1:Nsave).*conj(term2a(1:Nsave));
    term1=ampambg1'*ct+sqrt(-1)*(ampambg1'*st);
    term2=ampambg2'*ct+sqrt(-1)*(ampambg2'*st);
    ww1(irow,:)=phaseshift.*term1.*conj(term2);
    len=length(ampambg2);ww2(irow,:)=conv(ampambg1,ampambg2(len:-1:1))';
  end
  wS=col(rs);
  ww=ww1(1:length(rs),:);
  len=length(ww2(1,:));
  wL=t2-t1+(1:len)-(len+1)/2;
