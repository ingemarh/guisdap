% gup0L_design.m : ESR long pulse test experiment
% GUISDAP v1.60   96-05-27 Copyright Asko Huuskonen, Markku Lehtinen
% Day 1 code
% Four long pulses (360 us) 

Ncycles=4;

DS_start('ESR')

% The basic structure of the transmission is the following (Wannberg Jan 9, 1996)
%   40 BEAMON
%   80  360 us long pulse  
%  460  360 us long pulse  
%  840  360 us long pulse
% 1220  360 us long pulse
% 1600 BEAMOFF
% The basic cycle length is 2*6720 us, which is divided into two different halves
% The  first half: frequencies F1-F4 for are used for transmission 
%                  one (or two) of the frequencies  F5-F8 are calibrated 
% The second half: uses frequencies F5-F8 for transmission
%                  one (or two) from the other frequency set is calibrated
% Four cycles are needed to get all the background sampled
% REP will be 4x2x6720 us

% The program code looks neater if we use half of the basic cycle length as variable
cycle=6720;

% We define reception start time so that a 400 us delay is added after BEAMOFF
RECtime=2000; 
% The sampling windows are defined as follows
RECwindow=[2000 6199]; % 210 samples
CALwindow=[6300 6499]; % 10 samples


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
DS_set('channel',1)                  % We call this channel, but actually we mean frequency!!
DS_set('XMITtime',80,'bitlen',360)   % Transmit a 360 us long pulse at 80 us (mod 14 ms)

DS_set('taps',impresp);
DS_set('adcint',20)                  % Sampling is done at 20 us intervals
DS_set('RECwindow',RECwindow)        % Signal sampling window as defined above

DS_set('Bcycles',[1;4])              
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

DS_set('channel',2)
DS_set('XMITtime',460)
DS_set('Bcycles',[2;3])              

DS_set('go')

%************* Third long pulse   **************************
%
% Channels 5 and 6 repeat the transmission done on channels 1 and 2
% Now the transmission and signal reception is placed on the latter half of the cycle
% Background and calibration sampling is done during the first half of the cycle

DS_set('channel',5)
DS_set('XMITtime',cycle+80)

DS_set('RECwindow',cycle+RECwindow)

DS_set('Bcycles',[1;4])              
DS_set('Bwindow',RECwindow)          % Sampling during the first half of cycle
DS_set('Cwindow',CALwindow)          % Calibration sampling during the first half of the cycle

DS_set('go')


%************* Fourth long pulse   **************************
%
% We only need to specify the parameters changed
%

DS_set('channel',6)
DS_set('XMITtime',cycle+460) 

DS_set('Bcycles',[2;3])              

DS_set('go')
%

%************* Fifth long pulse ***********************
%

DS_set('channel',3)                  % We call this channel, but actually we mean frequency!!
DS_set('XMITtime',840,'bitlen',360)  % Transmit a 360 us long pulse at 80 us (mod 14 ms)

DS_set('RECwindow',RECwindow)        % Signal sampling window as defined above

DS_set('Bcycles',[2;3])              
DS_set('Bwindow',cycle+RECwindow)    % Sampling during the latter half of cycle
DS_set('Cwindow',cycle+CALwindow)    % Calibration sampling during the latter half of the cycle

DS_set('go')


%************* Sixth long pulse   **************************
%
% We only need to specify the parameters changed
%

DS_set('channel',4)
DS_set('XMITtime',1220)
DS_set('Bcycles',[1;4])              

DS_set('go')

%************* Seventh long pulse   **************************
%
% Channels 7 and 8 repeat the transmission done on channels 3 and 4
% Now the transmission and signal reception is placed on the latter half of the cycle
% Background and calibration sampling is done during the first half of the cycle

DS_set('channel',7)
DS_set('XMITtime',cycle+840)

DS_set('RECwindow',cycle+RECwindow)

DS_set('Bcycles',[2;3])              
DS_set('Bwindow',RECwindow)          % Sampling during the first half of cycle
DS_set('Cwindow',CALwindow)          % Calibration sampling during the first half of the cycle

DS_set('go')


%************* Eighth long pulse   **************************
%
% We only need to specify the parameters changed
%

DS_set('channel',8)
DS_set('XMITtime',cycle+1220) 

DS_set('Bcycles',[1;4])              

DS_set('go')
%
% Pulses on channels 6 and 8 are actually only 150 microseconds long, 
% but the lag profiles are calculated at for a 360 us pulse.
vcs=find(vc_ch==4 | vc_ch==8);
[M,N]=size(vc_env);
len=150/p_dtau;
vc_env(:,vcs)=[0;ones(len,1);zeros(M-len-1,1)]*ones(size(vcs));

% Mark the longer lag profile values as garbage
for vc=vcs
  lps=find((lp_vc==vc) & lp_t2-lp_t1>=len);
  lp_bcs(lps)='g'*ones(size(lps));
end