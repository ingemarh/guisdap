% XMIT_pulse: Xmits a general bit pattern on a given channel
% GUISDAP v.1.60 96-05-27 Copyright Asko Huuskonen and Markku Lehtinen
%
% Function to define transmitter waveforms
% input parameters
% ch        : physical channel for the transmission
% starttime : start time of the transmitted envelope (us)
% pattern1, pattern2: Define the waveform as Kronecker product of the two; 
% bitlength: is the basic bit length of the code in us
% Examples (B13 is a 13 bit Barker code)
% pattern1   pattern2 bitlength Comments 
%    1          1       350     Long pulse of 350 us, (see  XMIT_sp ) 
%    1          1        21     Power profile pulse of 21 us, (see  XMIT_sp ) 
%    1          B13       4     Barker coded power profile, bit length is 4 us 
%    [1 0 1]    1        20     Two pulse code, pulse length is 20 us 
%    [1 0 1]    B13       4     Barker coded two pulse code, bit length is 4 us 
%   alt code    1        21     Alternating code, bit length is 21 us 
%   alt code    B13       3     Barker coded alternating code, bit length is 3 us 
%   any seq.    1        10     Any sign sequence, bit length is 10 us 
%
% See also: design, XMIT_sp, XMIT_mp
% function XMIT_pulse(ch,starttime,pattern1,pattern2,bitlength)
  function XMIT_pulse(ch,starttime,pattern1,pattern2,bitlength)
  
global vc_number p_dtau vc_next vc_env vc_envo vc_ch vc_mf ch_gain ch_fradar ch_XMIT ch_REC 
  
% express all times in units of p_dtau  
starttime=starttime/p_dtau;
if abs(rem(bitlength,p_dtau)/p_dtau)>10*eps,
  fprintf(' Bit length MUST be an exact multiple of p_dtau\n')
  ch,starttime,pattern1,pattern2,bitlength
  error('')
else
  bitlength=round(bitlength/p_dtau);
end

vc_number=vc_next;
vc=vc_number;   % For local use

pattern1=pattern1(:); % Column vector
pattern2=pattern2(:);

env=kron(pattern1,kron(pattern2,ones(bitlength,1)));
index=wnz(env,1);
env=env(index);
starttime=starttime+index(1)-1;
len=length(env);

% Check now that no other channel is transmitting at the time
index=starttime+find(env);
[XMITch,RECch]=active_ch(index);
if length(XMITch)
  fprintf('\n\nERROR:\nClash in the transmission definitions:\n')
  fprintf('Channel %.0f has been asked to transmit ',ch)
  fprintf('from %.1f to %.1f, but\n', starttime*p_dtau, (starttime+len)*p_dtau)
  fprintf('channel %.0f is already on\n',XMITch)
  error(' ')
end

% Check further that no channel is receiving at the time of transmission
if length(RECch)
  fprintf('\n\nERROR:\nChannel %.0f has been asked to transmit ',ch)
  fprintf('from %.1f to %.1f, but\nchannel(s)', starttime*p_dtau, (starttime+len)*p_dtau)
  fprintf(' %.0f',RECch)
  fprintf(' receive(s) during the period\n\n')
  error(' ')
end

vc_env(1:len+2,vc)=[0;env;0];
vc_envo(vc)=starttime-1;
vc_ch(vc)=ch;
vc_mf(vc)=0;
len=length(ch_fradar);
if len<ch % No frequency allocated to this channel
  ch_fradar(len+1:ch)=ch_fradar(1)*ones(1,ch-len); % Copy the first one (which always exists)
  ch_gain(len+1:ch)=ch_gain(1)*ones(1,ch-len);
end

% Advance the virtual channel counter
vc_next=vc_number+1;
fprintf(' Xmission defined: Virtual channel %.0f, real channel %.0f\n',vc_number, ch)
