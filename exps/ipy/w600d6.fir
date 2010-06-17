FIRPAR_VS 0.1

% DDFplan 1.1 - assuming 10.0 MHz A/D
% 31-Aug-2002
% L,N=   3,2
% F_CLK > 1.5 x CLK_IN
% ddfplan(2,3,3,3,2)
%
% HDF pole1: 5000000.000 Hz
% FIR pole1: 2500000.000 Hz
H_STAGES 3
H_GROWTH 47

H_DRATE  1
H_BYP    0
F_CLA    0

F_TAPS   3
F_ESYM   1
F_DRATE  2
F_BYP    0
F_0AD    0

TAP 0 0x10000  % 0.1250
TAP 1 0x30000  % 0.3750
