FIRPAR_VS 0.1

% DDFplan 1.3 - assumes 15 MHz A/D
% 15-Mar-2007
% ddf15(3,4,3,2,2)
% -----------------------
% Total decimation 9
%  0.6 usec sampling interval
%  1667 kHz sampling rate
% 3dB point at 731.6 kHz
% Noise equivalent bandwidth 1535.5 kHz
% -----------------------
% DDF is equivalent to a cascade of:
%     4 boxcar FIRs with 3 taps each
%   + decimation by 3
%   + 2 boxcar FIRs with 2 taps each
%   + decimation by 3
% Speed OK: 30MHz > 1 * 15MHz
% HDF first pole at 5000.0 kHz
% FIR first pole at 2500.0 kHz

H_STAGES 4
H_GROWTH 43

H_DRATE  2
H_BYP    0
F_CLA    0

F_TAPS   2
F_ESYM   1
F_DRATE  2
F_BYP    0
F_0AD    0

TAP 0 0x32916  % 0.3951
TAP 1 0x6522C  % 0.7901
