FIRPAR_VS 0.1
%=========================================
% b42d180.fir
% +- 42 kHz filter for 12 usec sampling
%=========================================
% DDFplan 1.1 - assuming 15.0 MHz A/D
% 24-Nov-2000
% ddfplan15(18,5,10,5,4) -- 
% L,N=   5,4
% F_CLK > 0.122222 x CLK_IN
% HDF pole1: 833333.333 Hz
% FIR pole1: 208333.333 Hz

H_STAGES 5
H_GROWTH 29

H_DRATE  17
H_BYP    0
F_CLA    0

F_TAPS   15
F_ESYM   1
F_DRATE  9
F_BYP    0
F_0AD    0

TAP 0 0x00238  % 0.0011
TAP 1 0x00B19  % 0.0054
TAP 2 0x0214C  % 0.0163
TAP 3 0x04DB1  % 0.0379
TAP 4 0x09048  % 0.0704
TAP 5 0x0E031  % 0.1095
TAP 6 0x12BA9  % 0.1463
TAP 7 0x1580E  % 0.1680
