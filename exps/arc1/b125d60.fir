FIRPAR_VS 0.1

% DDFplan 1.3 - assumes 15 MHz A/D
% 15-Mar-2007
% ddf15(12,5,5,5,2)
% -----------------------
% Total decimation 60
%  4 usec sampling interval
%  250 kHz sampling rate
% 3dB point at 127.3 kHz
% Noise equivalent bandwidth 269.2 kHz
% -----------------------
% DDF is equivalent to a cascade of:
%     5 boxcar FIRs with 12 taps each
%   + decimation by 12
%   + 5 boxcar FIRs with 2 taps each
%   + decimation by 5
% Speed OK: 30MHz > 0.2 * 15MHz
% HDF first pole at 1250.0 kHz
% FIR first pole at 625.0 kHz

H_STAGES 5
H_GROWTH 32

H_DRATE  11
H_BYP    0
F_CLA    0

F_TAPS   5
F_ESYM   1
F_DRATE  4
F_BYP    0
F_0AD    0

TAP 0 0x0436D  % 0.0329
TAP 1 0x1511F  % 0.1646
TAP 2 0x2A23D  % 0.3292
