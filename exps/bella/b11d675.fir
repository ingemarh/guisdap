FIRPAR_VS 0.1

% DDFplan 1.3 - assumes 15 MHz A/D
% 15-Mar-2007
% ddf15(45,5,15,9,5)
% -----------------------
% Total decimation 675
%  45 usec sampling interval
%  22.22 kHz sampling rate
% 3dB point at 10.2 kHz
% Noise equivalent bandwidth 21.7 kHz
% -----------------------
% DDF is equivalent to a cascade of:
%     5 boxcar FIRs with 45 taps each
%   + decimation by 45
%   + 9 boxcar FIRs with 5 taps each
%   + decimation by 15
% Speed OK: 30MHz > 0.0563 * 15MHz
% HDF first pole at 333.3 kHz
% FIR first pole at 66.7 kHz

H_STAGES 5
H_GROWTH 22

H_DRATE  44
H_BYP    0
F_CLA    0

F_TAPS   36
F_ESYM   1
F_DRATE  14
F_BYP    0
F_0AD    0

TAP 0 0x00000  % 0.0000
TAP 1 0x00004  % 0.0000
TAP 2 0x00012  % 0.0000
TAP 3 0x00040  % 0.0001
TAP 4 0x000C1  % 0.0004
TAP 5 0x001F3  % 0.0010
TAP 6 0x00475  % 0.0022
TAP 7 0x00933  % 0.0045
TAP 8 0x0115E  % 0.0085
TAP 9 0x01E49  % 0.0148
TAP 10 0x03122  % 0.0240
TAP 11 0x04A8F  % 0.0364
TAP 12 0x06A48  % 0.0519
TAP 13 0x08EC6  % 0.0697
TAP 14 0x0B537  % 0.0885
TAP 15 0x0D9BB  % 0.1063
TAP 16 0x0F801  % 0.1211
TAP 17 0x10C0C  % 0.1309
TAP 18 0x11310  % 0.1343
