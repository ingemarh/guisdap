% XMIT_mp: Xmits a multipulse sequence on a given channel
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% Function to define multipulse sequences
% input parameters
% ch        : physical channel for the transmission
% starttime : start time of the transmitted envelope (us)
% pattern   : gives the pulse separations, e.g. 2513 for 5-pulse code
% bitlength, gaplength: basic unit in the transmission consists of pulse of
%                       length 'bitlength' and gap of length 'gaplength'
% bitseq    : Each bit in the multipulse code is coded with this bit sequence
%           : Used normally with Barker coding, value 1 assumed if not given
% example:   EISCAT CP1H-multipulse is obtained by
%                 XMIT_mp(ch,starttime,2513,14,16) or
%                 XMIT_mp(ch,starttime,[2,5,1,3],14,16) or
%
% See also: XMIT_pulse
%
% function XMIT_mp(ch,starttime,pattern,bitlength,gaplength,bitseq)
  function XMIT_mp(ch,starttime,pattern,bitlength,gaplength,bitseq)

global p_dtau

if nargin==5,
  bitseq=1;
end

if length(pattern)==1 % pattern given as one integer, must decode it first
  apu=pattern;
  pattern=[];
  while apu>0,
    pattern=[rem(apu,10), pattern];
    apu=floor(apu/10);
  end
end

pattern=[0,pattern];
pattern1(cumsum(pattern)+1)=ones(size(pattern));

% check that all times are multiples of p_dtau
if abs(rem(bitlength,p_dtau)/p_dtau)>10*eps ...
                         | abs(rem(gaplength,p_dtau)/p_dtau)>10*eps,
  fprintf(' Bit length MUST be an exact multiple of p_dtau\n')
  ch,starttime,pattern,bitlength,gaplength
  error('')
else
  bitlength=round(bitlength/p_dtau);
  gaplength=round(gaplength/p_dtau);
  pattern2=[kron(col(bitseq),ones(bitlength,1));zeros(gaplength,1)];
end

XMIT_pulse(ch,starttime,pattern1,pattern2,p_dtau)
