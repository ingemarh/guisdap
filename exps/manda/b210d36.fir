FIRPAR_VS 0.1

% DDFplan 1.3 - assumes 15 MHz A/D
% 15-Mar-2007
% ddf15(6,5,6,3,3)
% -----------------------
% Total decimation 36
%  2.4 usec sampling interval
%  416.7 kHz sampling rate
% 3dB point at 211.1 kHz
% Noise equivalent bandwidth 444.6 kHz
% -----------------------
% DDF is equivalent to a cascade of:
%     5 boxcar FIRs with 6 taps each
%   + decimation by 6
%   + 3 boxcar FIRs with 3 taps each
%   + decimation by 6
% Speed OK: 30MHz > 0.389 * 15MHz
% HDF first pole at 2500.0 kHz
% FIR first pole at 833.3 kHz

H_STAGES 5
H_GROWTH 37

H_DRATE  5
H_BYP    0
F_CLA    0

F_TAPS   6
F_ESYM   1
F_DRATE  5
F_BYP    0
F_0AD    0

TAP 0 0x04FE9  % 0.0390
TAP 1 0x0EFBB  % 0.1171
TAP 2 0x1DF75  % 0.2341
TAP 3 0x22F5E  % 0.2731
