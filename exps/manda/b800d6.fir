FIRPAR_VS 0.1

% DDFplan 1.3 - assumes 15 MHz A/D
% 15-Mar-2007
% ddf15(3,2,2,2,2)
% -----------------------
% Total decimation 6
%  0.4 usec sampling interval
%  2500 kHz sampling rate
% 3dB point at 806.5 kHz
% Noise equivalent bandwidth 1689.8 kHz
% -----------------------
% DDF is equivalent to a cascade of:
%     2 boxcar FIRs with 3 taps each
%   + decimation by 3
%   + 2 boxcar FIRs with 2 taps each
%   + decimation by 2
% Speed OK: 30MHz > 1.33 * 15MHz
% HDF first pole at 5000.0 kHz
% FIR first pole at 2500.0 kHz

H_STAGES 2
H_GROWTH 46

H_DRATE  2
H_BYP    0
F_CLA    0

F_TAPS   2
F_ESYM   1
F_DRATE  1
F_BYP    0
F_0AD    0

TAP 0 0x38E39  % 0.4444
TAP 1 0x71C72  % 0.8889
