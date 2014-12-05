FIRPAR_VS 0.1

% DDFplan 1.3 - assumes 15 MHz A/D
% 15-Mar-2007
% ddf15(5,5,6,3,3)
% -----------------------
% Total decimation 30
%  2 usec sampling interval
%  500 kHz sampling rate
% 3dB point at 253.5 kHz
% Noise equivalent bandwidth 534.0 kHz
% -----------------------
% DDF is equivalent to a cascade of:
%     5 boxcar FIRs with 5 taps each
%   + decimation by 5
%   + 3 boxcar FIRs with 3 taps each
%   + decimation by 6
% Speed OK: 30MHz > 0.467 * 15MHz
% HDF first pole at 3000.0 kHz
% FIR first pole at 1000.0 kHz

H_STAGES 5
H_GROWTH 38

H_DRATE  4
H_BYP    0
F_CLA    0

F_TAPS   6
F_ESYM   1
F_DRATE  5
F_BYP    0
F_0AD    0

TAP 0 0x0636C  % 0.0485
TAP 1 0x12A43  % 0.1456
TAP 2 0x25486  % 0.2913
TAP 3 0x2B7F2  % 0.3398
