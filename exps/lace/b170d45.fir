FIRPAR_VS 0.1

% DDFplan 1.3 - assumes 15 MHz A/D
% 15-Mar-2007
% ddf15(9,5,5,5,2)
% -----------------------
% Total decimation 45
%  3 usec sampling interval
%  333.3 kHz sampling rate
% 3dB point at 169.8 kHz
% Noise equivalent bandwidth 359.2 kHz
% -----------------------
% DDF is equivalent to a cascade of:
%     5 boxcar FIRs with 9 taps each
%   + decimation by 9
%   + 5 boxcar FIRs with 2 taps each
%   + decimation by 5
% Speed OK: 30MHz > 0.267 * 15MHz
% HDF first pole at 1666.7 kHz
% FIR first pole at 833.3 kHz

H_STAGES 5
H_GROWTH 34

H_DRATE  8
H_BYP    0
F_CLA    0

F_TAPS   5
F_ESYM   1
F_DRATE  4
F_BYP    0
F_0AD    0

TAP 0 0x04708  % 0.0347
TAP 1 0x16328  % 0.1734
TAP 2 0x2C64F  % 0.3468
