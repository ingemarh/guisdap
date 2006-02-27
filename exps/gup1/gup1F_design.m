% GUISDAP v1.53 95-01-10
% 
% Written by Asko Huuskonen
% Day 1 code
% Two long pulses (360 us) and two alternating codes (12 x 40 us)
% The alternating code set consists of 32 codes

Ncycles=32; % This should be 32 
if Ncycles<32,
  fprintf(' The value of Ncycles (%.0f) is too small (value useful to see that timing is OK)\n',Ncycles)
  fprintf(' Change it back to 32 when the timing diagram looks fine\n')
  fprintf(' Press any key to continue\n')
  pause
end
DS_start('ESR',Ncycles*1000,23)
p_dtau=2; % Use 2 us as the basic time unit

% The basic structure of the transmission is the following (Wannberg Jan 9, 1996)
%   40 BEAMON
%   80  360 us long pulse  
%  460  360 us long pulse  
%  840  12x40 us alternating code
% 1340  12x40 us alternting code
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

% This source code is able to produce two different result memory layout
% depending on whether data from similar modulations is added to same
% memory locations or not
% ADD = 1 ; %similar data added
 ADD = 0 ; %all data kept separate

% In addition, it is possibe to keep data from
% the two hardware background channel boards separate
% The each signal data block will have two background/calibration blocks
Bcycles=[1:4:Ncycles;... 
         3:4:Ncycles];
% In the final version all similar data is added together, then
% Bcycles=1:2:Ncycles;

% We define reception start time so that a 360 us delay is added after BEAMOFF
RECdelay=360;
RECtime=1820+RECdelay; % 2180
% The sampling windows are defined as follows
RECwindow=[RECtime  cycle-501]; % [2180 6499] for signal and background sampling
CALwindow=[cycle-460 cycle-21]; % [6540 6979] for calibration sampling


% These are the filter taps designed by Jussi
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


if ADD, ra_next=0;  end    % Low level command to use same memory locations again
                           % Will be replaced by DS_set-call later
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


if ADD, ra=ra_next;  end    % Low level command to use same memory locations again
                            % Will be replaced by DS_set-call later
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
DS_set('Sgates',141)                 % 140 gates (2800 us = 420 km) is sufficient

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

if ADD, ra_next=ra;  end    % Low level command to use same memory locations again
                            % Will be replaced by DS_set-call later
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
