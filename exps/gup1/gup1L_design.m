% gup1L_design.m : ESR alternating code/long pulse test experiment
% GUISDAP v1.60   96-05-27 Copyright Asko Huuskonen, Markku Lehtinen
% Day 1 code
% Two long pulses (360 us) and two alternating codes (12 x 40 us)
% The alternating code set consists of 32 codes

Ncycles=32; % Number of codes in the alternating code set
DS_start('ESR',Ncycles*1000,23)

% The basic structure of the transmission is the following (Wannberg Jan 9, 1996)
%   40 BEAMON
%   80  360 us long pulse  
%  460  360 us long pulse  
%  840  12x40 us alternating code
% 1340  12x40 us alternting code  *********  NOTE 6x40 us code used finally
% 1820 BEAMOFF
% The basic cycle length is 14 ms, which is divided into two different halves
% The  first half: frequencies F5, F6, F1 and F2 for are used for transmission 
%                  one (or two) of the frequencies  F7, F8, F3 and F4 are calibrated 
% The second half: uses frequencies F7, F8, F3 and F4 for transmission
%                  one (or two) from the other frequency set is calibrated
% The alternating code set contains 32 different codes, therefore the
% REP will be 32x14 ms = 448 ms.

% The program code looks neater if we use half of the basic cycle length as variable
cycle=7000;

% Background data from the two hardware background channel boards are kept separate
% The each signal data block will have two background/calibration blocks
Bcycles=[1:4:Ncycles;... 
         3:4:Ncycles];

RECtime=2180; %This is when reception is started
% The sampling windows are defined as follows
RECwindow=[RECtime  cycle-501]; % [2180 6499] for signal and background sampling
CALwindow=[cycle-460 cycle-21]; % [6540 6979] for calibration sampling


% These are the filter taps designed by Jussi Markkanen
x=[  13  64  192  448  832  1293 1728 1984];
impresp=[x, fliplr(x)];
% As the taps are given with a 2 us resolution, we define
p_dtau=2;



%************* FIRST CODE starts here ***********************
%
% Definitions for the first code
% 

DS_set('clear')                      % Clear the internal variables first
DS_set('channel',5)                  % We call this channel, but actually we mean frequency!!
DS_set('XMITtime',80,'bitlen',360)   % Transmit a 360 us long pulse at 80 us (mod 14 ms)

DS_set('taps',impresp);
DS_set('adcint',20)                  % Sampling is done at 20 us intervals
DS_set('RECwindow',RECwindow)        % Signal sampling window as defined above

DS_set('Bcycles',Bcycles+1)          % Use the second cycle and every second thereafter
DS_set('Bwindow',cycle+RECwindow)    % Sampling during the latter half of cycle
DS_set('Blags',0:20:340)             % Calculate all lags during the test period
DS_set('Cwindow',cycle+CALwindow)    % Calibration sampling during the latter half of the cycle

DS_set('cycle',2*cycle)              % The cycle is 14 ms. All is repeated again mod 14 ms
DS_set('Ncycles',Ncycles)            % The AC part will require 32 cycles to complete.

                  % ************* ALL is set, GO! ******************
                  % Everything else is cleverly guessed
                  % Filter impulse response is matched with the sampling interval
                  % All signal lags are calculated
                  % Only zero lag calculated for calibration
DS_set('go')


%************* Second long pulse   **************************
%
% We only need to specify the parameters changed
%

DS_set('channel',6)
DS_set('XMITtime',460)

DS_set('go')

%************* Third long pulse   **************************
%
% Channels 7 and 8 repeat the transmission done on channels 5 and 6
% Now the transmission and signal reception is placed on the latter half of the cycle
% Background and calibration sampling is done during the first half of the cycle

DS_set('channel',7)
DS_set('XMITtime',cycle+80)

DS_set('RECwindow',cycle+RECwindow)

DS_set('Bwindow',RECwindow)          % Sampling during the first half of cycle
DS_set('Cwindow',CALwindow)          % Calibration sampling during the first half of the cycle

DS_set('go')


%************* Fourth long pulse   **************************
%
% We only need to specify the parameters changed
%

DS_set('channel',8)
DS_set('XMITtime',cycle+460) 

DS_set('go')
%
%************* All long pulses have been spefied now! *******


%************* FIRST Alternating code starts here **********
%
% Definitions for the first code
%

DS_set('clear')                      % It is best the clear all definitions here
DS_set('channel',1)                  % We call this channel, but actually we mean frequency!!
DS_set('XMITtime',840)               % The alternating code transmission starts at 840 us
DS_set('Nbits',12,'bitlen',40)       % A 12-bit code with 40 us bit length is used

DS_set('adcint',20)                  % Sampling is done at 20 us intervals
DS_set('RECtime',RECwindow(1))       % Start sampling simultaneously with the long pulse
DS_set('Gates',141)                 % 140 gates (2800 us = 420 km) is sufficient

DS_set('Bcycles',Bcycles)            % Use the first cycle and every second thereafter
DS_set('Bwindow',cycle+RECwindow)    % Sampling during the latter half of cycle
DS_set('Blags',0:20:460)             % Calculate all lags during the test period
DS_set('Cwindow',cycle+CALwindow)    % Calibration sampling during the latter half of the cycle

DS_set('cycle',2*cycle)              % The cycle is 14 ms. All is repeated again mod 14 ms
DS_set('Ncycles',Ncycles)            % The AC part will require 32 cycles to complete.

                  % ************* ALL is set, GO! ******************
                  % Everything else is cleverly guessed
                  % Filter impulse response is matched with the sampling interval
                  % All signal lags are calculated
                  % Only zero lag calculated for calibration
DS_set('go')


%************* Second Alternating code  ******************
%
% We only need to specify the parameters changed
%

DS_set('channel',2)
DS_set('XMITtime',1340)

DS_set('go')

%************* Third Alternating code  ******************
%
% Channels 3 and 4 repeat the transmission done on channels 1 and 2
% Now the transmission and signal reception is placed on the latter half of the cycle
% Background and calibration sampling is done during the first half of the cycle

DS_set('channel',3)
DS_set('XMITtime',cycle+840)

DS_set('RECtime',cycle+RECwindow(1)) % Sampling during the latter half of cycle

DS_set('Bwindow',RECwindow)          % Sampling during the first half of cycle
DS_set('Cwindow',CALwindow)          % Calibration sampling during the first half of the cycle

DS_set('go')


%************* Fourth Alternating code  ******************
%
% We only need to specify the parameters changed
%

DS_set('channel',4)
DS_set('XMITtime',cycle+1340)

DS_set('go')

%
%************* All alternating codes specified *************


% The alternating codes on channels 2 and 4 where cut to 6 bit codes
% in order to reduce the ground clutter observed at ranges below 100 km.
% The DSP calculated the same lag profiles are for the 12 bit code
% Here we make the necessary  changes to the transmitter envelopes and
% the lag profile definitions so that the experiment definition agrees
% with the actual experiment used

% Cut the envelopes
vc=find(vc_ch==2 | vc_ch==4); % The last alternating code is on channels 2 and 4
ind=(2+480/p_dtau/2):rows(vc_env); % 
vc_env(ind,vc)=zeros(length(ind),length(vc));

% Then find all the signal lag profiles calculated from the pulses
lp=find((vc_ch(lp_vc(1:lp_ind))==2 | vc_ch(lp_vc(1:lp_ind))==4 ) & lp_bcs(1:lp_ind)=='s');
% Change those lag profile filter coefficients to zero
% which originate from the removed 6 bits in the code
lp_nfir(lp)=lp_nfir(lp)-12; % Each filter will be shorter by 2*6
for len=1:2:9
  ind=find(lp_nfir(lp)==len);
  lp_fir((len+1):21,lp(ind))=zeros(21-len,length(ind));
end

% Mark as garbage those whose filter length is less than zero after subraction
ind=find(lp_nfir<=0);
lp_bcs(ind)='g'*ones(size(ind));
lp_nfir(ind)=zeros(size(ind));

