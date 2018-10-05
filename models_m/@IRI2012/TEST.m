function rtn = TEST( testCase )
%IRITEST test program for the iri_web subroutine
% iritest.for, version number can be found at the end of this comment.
%-----------------------------------------------------------------------
%
% test program for the iri_web subroutine
%
%-version-mm/dd/yy ----------corrections--------------------------
% 2000.01 05/07/01 initial version
% 2000.02 07/11/01 line 210: do i=1,100 instead of i=2,100 (K. Tokar)
% 2000.03 28/12/01 output oar(39) for IG12 (R. Conkright, NGDC, NOAA)
% 2000.04 28/10/02 replace TAB/6 blanks, enforce 72/line (D. Simpson)
% 2000.05 02/06/03 Ne(Te) only 300,400; foF1 and hmF1 output corr.
% 2000.06 01/19/05 (~jf(20)) instead of (..jf(2)) (G. Schiralli)
% 2005.01 05/06/06 included spread-F (jf(28)) and topside (jf(29)) options
% 2007.00 05/18/07 Release of IRI-2007
% 2007.02 10/31/08 outf(100) -> outf(500), numhei=numstp=500
% 2007.03 02/12/09 added new D-region option (h=-3)
% 2007.11 04/19/10 correct TEC for normal output  [Shunrong Zhang] 
%
% 2012.00 10/05/11 IRI-2012: bottomside B0 B1 model (SHAMDB0D, SHAB1D),
% 2012.00 10/05/11    bottomside Ni model (iriflip.for), auroral foE
% 2012.00 10/05/11    storm model (storme_ap), Te with PF10.7 (elteik),
% 2012.00 10/05/11    oval kp model (auroral_boundary), IGRF-11(igrf.for), 
% 2012.00 10/05/11    NRLMSIS00 (cira.for), CGM coordinates, F10.7 daily
% 2012.00 10/05/11    81-day 365-day indices (apf107.dat), ap->kp (ckp),
% 2012.00 10/05/11    array size change jf(50) outf(20,1000), oarr(100).
% 2012.00 03/21/12    piktab=4 output for D-region
% 2012.01 09/16/12 Corrected UT output (hour-25)
%
%      INTEGER           pad1(6),jdprof(77),piktab
%      DIMENSION         outf(20,1000),oar(100,1000),jfi(6)
%      LOGICAL        jf(50)
%      CHARACTER*2       timev(2)
%      CHARACTER*3       uni(58),sopt
%      CHARACTER*4       IMZ(8),MAP,xtex,coorv(2)
%      CHARACTER*5       ITEXT(8)
%      CHARACTER*6       dopt,pna(58)
%      CHARACTER*8       bopt,topt
%      CHARACTER*9       pname(6)
%      CHARACTER*11      iopt
%      CHARACTER*16      f1opt
% should get this output:
% 
% 
% 
% yyyy/mmdd(or -ddd)/hh.h):2012/ 921/16.0UT  geog Lat/Long/Alt= 40.0/ 256.0/   0.0
%   90.6023178101  66.2600021362
% 
% oar:   1     0.6646292808E+12
% oar:   2     0.2595280457E+03
% oar:   3    -0.1000000000E+01
% oar:   4    -0.1000000000E+01
% oar:   5     0.1214685921E+12
% oar:   6     0.1100000000E+03
% oar:   7     0.7570081280E+09
% oar:   8     0.8110861206E+02
% oar:   9     0.0000000000E+00
% oar:  10     0.9060231781E+02
% oar:  11     0.1147009270E+12
% oar:  12     0.1268443832E+03
% oar:  13     0.1620805054E+04
% oar:  14     0.2100216370E+03
% oar:  15     0.2110117676E+04
% oar:  16     0.2698966064E+04
% oar:  17     0.3691529297E+04
% oar:  18     0.3983890869E+04
% oar:  19     0.4341105469E+04
% oar:  20     0.3552767029E+03
% oar:  21     0.1221048706E+04
% oar:  22     0.1348250000E+04
% oar:  23     0.5507208633E+02
% oar:  24     0.1518288255E-01
% oar:  25     0.6675817871E+02
% oar:  26     0.4933974457E+02
% oar:  27     0.5308678055E+02
% oar:  28     0.4000000000E+02
% oar:  29     0.4438767433E+01
% oar:  30     0.1931497574E+02
% oar:  31     0.3000000000E+01
% oar:  32     0.2560000000E+03
% oar:  33     0.6626000214E+02
% oar:  34     0.1158947296E+03
% oar:  35     0.2332311153E+01
% oar:  36     0.3110804796E+01
% oar:  37     0.1868008236E+18
% oar:  38     0.7280077362E+02
% oar:  39     0.6930000305E+02
% oar:  40     0.4011793137E+00
% oar:  41     0.1178000031E+03
% oar:  42     0.3887836933E+00
% oar:  43     0.2650000000E+03
% oar:  44    -0.1000000000E+01
% oar:  45     0.1000000000E+01
% oar:  46     0.1196999969E+03
% oar:  47     0.1000000000E+01
% oar:  48    -0.1000000000E+01
% oar:  49     0.4816670990E+02
% oar:  50     0.3229719849E+03
% oar:  51     0.1200000000E+02
% oar:  52     0.6000000000E+01
% oar:  53     0.4857408524E+02
% oar:  54     0.8856872559E+01
% oar:  55     0.4869116974E+02
% oar:  56     0.3231335144E+03
% oar:  57     0.8806015015E+01
% oar:  58     0.6642340851E+02
% oar:  59     0.6009999847E+02
% oar:  60     0.6076666641E+02
% oar:  61     0.6110000229E+02
% oar:  62     0.6106666565E+02
% oar:  63     0.6129999924E+02
% oar:  64     0.6233333206E+02
% oar:  65     0.6346666718E+02
% oar:  66     0.6430000305E+02
% oar:  67     0.6519999695E+02
% oar:  68     0.6663333130E+02
% oar:  69     0.6823332977E+02
% oar:  70     0.7006666565E+02
% oar:  71     0.7129999542E+02
% oar:  72     0.7153333282E+02
% oar:  73     0.7006666565E+02
% oar:  74     0.6783333588E+02
% oar:  75     0.6619999695E+02
% oar:  76     0.6469999695E+02
% oar:  77     0.6320000076E+02
% oar:  78     0.6203333282E+02
% oar:  79     0.6143333054E+02
% oar:  80     0.6093333435E+02
% oar:  81     0.6036666489E+02
% oar:  82     0.5993333435E+02
% oar:  83     0.2666666746E+01
% oar:  84     0.7686879158E+01
% NeQuick is used for topside Ne profile
% URSI maps are used for the F2 peak density (NmF2)
% CCIR maps are used for the F2 peak height (hmF2)
% IRI-95 option is used for D-region
% ABT-2009 option is used for the bottomside thickness parameter B0
% The foF2 STORM model is turned on 
% Scotto-97 no L   option is used for the F1 occurrence probability
% TBT-2011 option is used for the electron temperature
% RBY10+TTS03 option is used for ion composition
% 
% Peak Densities/cm-3: NmF2=   664629.25000   NmF1=        0.00000   NmE= 121468.59375
% Peak Heights/km:     hmF2=   259.52805   hmF1=     0.00000   hmE=   110.00000
% 
% Solar Zenith Angle/degree                              55.1
% Dip (Magnetic Inclination)/degree                     66.76
% Modip (Modified Dip)/degree                           53.09
% Solar Sunspot Number (12-months running mean) Rz12     66.3
% Ionospheric-Effective Solar Index IG12                 69.3
% 
% TEC [1.E16 m-2] is obtained by numerical integration in 1km steps
%   from 50 to 2000.0 km.  t is the percentage of TEC above the F peak.
% 
% -
%    H   ELECTRON DENSITY   TEMPERATURES         ION PERCENTAGES/%     1E16m-2
%    km  Ne/cm-3 Ne/NmF2 Tn/K  Ti/K  Te/K  O+  N+  H+ He+ O2+ NO+ Clust TEC t/%
%    0.0      -1 -1.000   293   293   293  -1  -1  -1  -1  -1  -1  -1  18.7  73
%   10.0      -1 -1.000   232   232   232  -1  -1  -1  -1  -1  -1  -1  18.7  73
%   20.0      -1 -1.000   215   215   215  -1  -1  -1  -1  -1  -1  -1  18.7  73
%   30.0      -1 -1.000   224   224   224  -1  -1  -1  -1  -1  -1  -1  18.7  73
%   40.0      -1 -1.000   249   249   249  -1  -1  -1  -1  -1  -1  -1  18.7  73
%   50.0      -1 -1.000   259   259   259  -1  -1  -1  -1  -1  -1  -1  18.7  73
%   60.0      -1 -1.000   240   240   240  -1  -1  -1  -1  -1  -1  -1  18.7  73
%   70.0     291  0.000   220   220   220  -1  -1  -1  -1  -1  -1  -1  18.7  73
%   80.0     717  0.001   195   195   195   0   0   0   0   1  99  -1  18.7  73
%   90.0   14513  0.022   177   177   177   0   0   0   0  12  88  -1  18.7  73
%  100.0   98666  0.148   192   192   192   0****   0   0********  -1  18.7  73
%  110.0  121469  0.183   274   274   274********   0   0********  -1  18.7  73
%  120.0  114707  0.173   355   355   355********   0   0********  -1  18.7  73
%  130.0  124196  0.187   479   479   496********   0   0********  -1  18.7  73
%  140.0  133788  0.201   586   586   636********   0   0********  -1  18.7  73
%  150.0  145295  0.219   668   668   777********   0   0********  -1  18.7  73
%  160.0  159836  0.240   732   732   918********   0   0********  -1  18.7  73
%  170.0  180392  0.271   781   781  1058********   0   0********  -1  18.7  73
%  180.0  225290  0.339   820   820  1199********   0   0********  -1  18.7  73
%  190.0  296048  0.445   849   849  1338********   0   0********  -1  18.7  73
%  200.0  373117  0.561   872   872  1473********   0   0********  -1  18.7  73
%  210.0  451340  0.679   890   890  1584********   0   0********  -1  18.7  73
%  220.0  524502  0.789   904   904  1649********   0   0********  -1  18.7  73
%  230.0  586315  0.882   915   918  1690********   0   0********  -1  18.7  73
%  240.0  631616  0.950   923   933  1725********   0   0********  -1  18.7  73
%  250.0  657522  0.989   930   948  1761********   0   0********  -1  18.7  73
%  260.0  664620  1.000   935   963  1796********   0   0********  -1  18.7  73
%  270.0  660070  0.993   939   978  1830********   0   0********  -1  18.7  73
%  280.0  648077  0.975   942   994  1865********   0   0********  -1  18.7  73
%  290.0  629996  0.948   945  1009  1900********   0   0********  -1  18.7  73
%  300.0  607210  0.914   947  1024  1935********   0   0********  -1  18.7  73
%  310.0  581028  0.874   948  1039  1970  98   1   0   0   0   0  -1  18.7  73
%  320.0  552622  0.831   949  1054  2005  98   1   0   0   0   0  -1  18.7  73
%  330.0  522991  0.787   950  1069  2040  98   1   0   0   0   0  -1  18.7  73
%  340.0  492949  0.742   951  1085  2075  98   2   0   0   0   0  -1  18.7  73
%  350.0  463137  0.697   952  1100  2108  98   2   0   0   0   0  -1  18.7  73
%  360.0  434038  0.653   952  1115  2139  98   2   0   0   0   0  -1  18.7  73
%  370.0  405997  0.611   952  1130  2169  98   2   0   0   0   0  -1  18.7  73
%  380.0  379248  0.571   953  1145  2198  98   2   0   0   0   0  -1  18.7  73
%  390.0  353935  0.533   953  1161  2228  98   2   0   0   0   0  -1  18.7  73
%  400.0  330132  0.497   953  1176  2257  98   2   0   0   0   0  -1  18.7  73
%  410.0  307860  0.463   953  1193  2287  98   2   0   0   0   0  -1  18.7  73
%  420.0  287101  0.432   953  1210  2316  98   2   0   0   0   0  -1  18.7  73
%  430.0  267811  0.403   953  1231  2346  97   2   0   0   0   0  -1  18.7  73
%  440.0  249927  0.376   953  1255  2375  97   2   0   0   0   0  -1  18.7  73
%  450.0  233374  0.351   953  1282  2405  97   2   0   0   0   0  -1  18.7  73
%  460.0  218072  0.328   954  1311  2434  97   2   0   0   0   0  -1  18.7  73
%  470.0  203938  0.307   954  1340  2463  97   2   0   0   0   0  -1  18.7  73
%  480.0  190888  0.287   954  1370  2493  97   2   0   0   0   0  -1  18.7  73
%  490.0  178841  0.269   954  1400  2522  97   2   0   1   0   0  -1  18.7  73
%  500.0  167721  0.252   954  1430  2552  97   2   1   1   0   0  -1  18.7  73
%  510.0  157453  0.237   954  1459  2581  96   2   1   1   0   0  -1  18.7  73
%  520.0  147968  0.223   954  1489  2611  96   2   1   1   0   0  -1  18.7  73
%  530.0  139202  0.209   954  1519  2641  96   3   1   1   0   0  -1  18.7  73
%  540.0  131096  0.197   954  1549  2671  96   3   1   1   0   0  -1  18.7  73
%  550.0  123594  0.186   954  1579  2701  96   3   1   1   0   0  -1  18.7  73
%  560.0  116647  0.176   954  1608  2733  95   3   1   1   0   0  -1  18.7  73
%  570.0  110207  0.166   954  1638  2766  95   3   1   1   0   0  -1  18.7  73
%  580.0  104233  0.157   954  1668  2798  95   3   1   1   0   0  -1  18.7  73
%  590.0   98686  0.148   954  1698  2831  95   3   1   1   0   0  -1  18.7  73
%  600.0   93530  0.141   954  1728  2864  94   3   2   1   0   0  -1  18.7  73
%  610.0   88733  0.134   954  1757  2897  94   3   2   1   0   0  -1  18.7  73
%  620.0   84267  0.127   954  1787  2931  94   3   2   1   0   0  -1  18.7  73
%  630.0   80103  0.121   954  1817  2964  93   3   2   1   0   0  -1  18.7  73
%  640.0   76219  0.115   954  1847  2997  93   3   3   1   0   0  -1  18.7  73
%  650.0   72591  0.109   954  1877  3030  93   3   3   1   0   0  -1  18.7  73
%  660.0   69199  0.104   954  1906  3063  92   3   3   1   0   0  -1  18.7  73
%  670.0   66026  0.099   954  1936  3096  92   3   3   1   0   0  -1  18.7  73
%  680.0   63053  0.095   954  1966  3129  92   3   3   1   0   0  -1  18.7  73
%  690.0   60266  0.091   954  1996  3162  91   3   4   1   0   0  -1  18.7  73
%  700.0   57651  0.087   954  2025  3195  91   4   4   1   0   0  -1  18.7  73
%  710.0   55194  0.083   954  2055  3228  91   4   4   2   0   0  -1  18.7  73
%  720.0   52885  0.080   954  2085  3261  91   4   4   2   0   0  -1  18.7  73
%  730.0   50711  0.076   954  2115  3294  90   4   4   2   0   0  -1  18.7  73
%  740.0   48665  0.073   954  2145  3327  90   4   5   2   0   0  -1  18.7  73
%  750.0   46735  0.070   954  2174  3360  90   4   5   2   0   0  -1  18.7  73
%  760.0   44915  0.068   954  2204  3393  89   4   5   2   0   0  -1  18.7  73
%  770.0   43196  0.065   954  2234  3426  89   4   5   2   0   0  -1  18.7  73
%  780.0   41572  0.063   954  2264  3458  89   4   6   2   0   0  -1  18.7  73
%  790.0   40035  0.060   954  2294  3490  88   4   6   2   0   0  -1  18.7  73
%  800.0   38581  0.058   954  2323  3522  88   4   6   2   0   0  -1  18.7  73
%  810.0   37204  0.056   954  2353  3552  88   4   6   2   0   0  -1  18.7  73
%  820.0   35898  0.054   954  2383  3581  87   4   7   2   0   0  -1  18.7  73
%  830.0   34659  0.052   954  2413  3608  87   4   7   2   0   0  -1  18.7  73
%  840.0   33483  0.050   954  2443  3632  86   4   7   3   0   0  -1  18.7  73
%  850.0   32365  0.049   954  2472  3653  86   4   8   3   0   0  -1  18.7  73
%  860.0   31302  0.047   954  2502  3671  86   4   8   3   0   0  -1  18.7  73
%  870.0   30291  0.046   954  2532  3685  85   4   8   3   0   0  -1  18.7  73
%  880.0   29329  0.044   954  2562  3696  85   4   9   3   0   0  -1  18.7  73
%  890.0   28412  0.043   954  2592  3706  84   4   9   3   0   0  -1  18.7  73
%  900.0   27537  0.041   954  2621  3714  84   4   9   3   0   0  -1  18.7  73
%  910.0   26703  0.040   954  2651  3721  83   4   9   3   0   0  -1  18.7  73
%  920.0   25907  0.039   954  2681  3727  83   4  10   3   0   0  -1  18.7  73
%  930.0   25147  0.038   954  2711  3733  82   4  10   3   0   0  -1  18.7  73
%  940.0   24420  0.037   954  2741  3739  82   4  10   4   0   0  -1  18.7  73
%  950.0   23725  0.036   954  2770  3744  82   4  11   4   0   0  -1  18.7  73
%  960.0   23060  0.035   954  2800  3750  81   4  11   4   0   0  -1  18.7  73
%  970.0   22424  0.034   954  2830  3755  81   4  11   4   0   0  -1  18.7  73
%  980.0   21814  0.033   954  2860  3761  80   4  12   4   0   0  -1  18.7  73
%  990.0   21230  0.032   954  2889  3766  80   4  12   4   0   0  -1  18.7  73
% 1000.0   20669  0.031   954  2919  3771  79   4  12   4   0   0  -1  18.7  73
% 1010.0   20132  0.030   954  2949  3777  79   4  12   4   0   0  -1  18.7  73
% 1020.0   19615  0.030   954  2979  3782  79   4  13   5   0   0  -1  18.7  73
% 1030.0   19120  0.029   954  3009  3787  78   4  13   5   0   0  -1  18.7  73
% 1040.0   18644  0.028   954  3038  3793  78   4  13   5   0   0  -1  18.7  73
% 1050.0   18186  0.027   954  3068  3798  77   4  14   5   0   0  -1  18.7  73
% 1060.0   17745  0.027   954  3098  3803  77   4  14   5   0   0  -1  18.7  73
% 1070.0   17322  0.026   954  3128  3808  76   4  15   5   0   0  -1  18.7  73
% 1080.0   16914  0.025   954  3158  3814  75   4  15   6   0   0  -1  18.7  73
% 1090.0   16521  0.025   954  3187  3819  75   4  15   6   0   0  -1  18.7  73
% 1100.0   16142  0.024   954  3217  3824  74   4  16   6   0   0  -1  18.7  73
% 1110.0   15778  0.024   954  3247  3830  74   4  16   6   0   0  -1  18.7  73
% 1120.0   15426  0.023   954  3277  3835  73   4  16   6   0   0  -1  18.7  73
% 1130.0   15086  0.023   954  3307  3840  73   4  17   6   0   0  -1  18.7  73
% 1140.0   14759  0.022   954  3336  3846  72   4  17   7   0   0  -1  18.7  73
% 1150.0   14442  0.022   954  3366  3851  72   4  18   7   0   0  -1  18.7  73
% 1160.0   14137  0.021   954  3396  3856  71   4  18   7   0   0  -1  18.7  73
% 1170.0   13842  0.021   954  3426  3862  70   4  18   7   0   0  -1  18.7  73
% 1180.0   13556  0.020   954  3456  3867  70   4  19   7   0   0  -1  18.7  73
% 1190.0   13281  0.020   954  3485  3872  69   4  19   8   0   0  -1  18.7  73
% 1200.0   13014  0.020   954  3515  3878  68   4  20   8   0   0  -1  18.7  73
% 1210.0   12755  0.019   954  3545  3883  68   4  20   8   0   0  -1  18.7  73
% 1220.0   12505  0.019   954  3575  3888  67   4  21   8   0   0  -1  18.7  73
% 1230.0   12263  0.018   954  3605  3894  66   4  21   9   0   0  -1  18.7  73
% 1240.0   12029  0.018   954  3634  3899  66   4  21   9   0   0  -1  18.7  73
% 1250.0   11802  0.018   954  3664  3904  65   4  22   9   0   0  -1  18.7  73
% 1260.0   11581  0.017   954  3694  3909  64   4  22   9   0   0  -1  18.7  73
% 1270.0   11368  0.017   954  3724  3915  64   4  23  10   0   0  -1  18.7  73
% 1280.0   11161  0.017   954  3753  3920  63   4  23  10   0   0  -1  18.7  73
% 1290.0   10960  0.016   954  3783  3925  62   4  24  10   0   0  -1  18.7  73
% 1300.0   10765  0.016   954  3813  3931  62   4  24  10   0   0  -1  18.7  73
% 1310.0   10575  0.016   954  3842  3936  61   4  25  11   0   0  -1  18.7  73
% 1320.0   10391  0.016   954  3871  3941  60   4  25  11   0   0  -1  18.7  73
% 1330.0   10213  0.015   954  3899  3947  59   4  26  11   0   0  -1  18.7  73
% 1340.0   10039  0.015   954  3923  3952  59   4  26  11   0   0  -1  18.7  73
% 1350.0    9870  0.015   954  3943  3957  58   4  27  12   0   0  -1  18.7  73
% 1360.0    9706  0.015   954  3957  3963  57   4  27  12   0   0  -1  18.7  73
% 1370.0    9547  0.014   954  3966  3968  56   4  28  12   0   0  -1  18.7  73
% 1380.0    9392  0.014   954  3973  3974  56   4  28  13   0   0  -1  18.7  73
% 1390.0    9241  0.014   954  3979  3979  55   4  29  13   0   0  -1  18.7  73
% 1400.0    9094  0.014   954  3985  3985  54   4  29  13   0   0  -1  18.7  73
% 1410.0    8951  0.013   954  3991  3991  53   4  30  14   0   0  -1  18.7  73
% 1420.0    8812  0.013   954  3997  3997  52   4  30  14   0   0  -1  18.7  73
% 1430.0    8676  0.013   954  4002  4002  52   4  31  14   0   0  -1  18.7  73
% 1440.0    8544  0.013   954  4008  4008  51   4  31  15   0   0  -1  18.7  73
% 1450.0    8415  0.013   954  4014  4014  50   3  32  15   0   0  -1  18.7  73
% 1460.0    8290  0.012   954  4020  4020  49   3  32  15   0   0  -1  18.7  73
% 1470.0    8167  0.012   954  4026  4026  49   3  33  15   0   0  -1  18.7  73
% 1480.0    8048  0.012   954  4032  4032  48   3  33  16   0   0  -1  18.7  73
% 1490.0    7932  0.012   954  4038  4038  47   3  34  16   0   0  -1  18.7  73
% 1500.0    7818  0.012   954  4044  4044  46   3  34  16   0   0  -1  18.7  73
% 1510.0    7707  0.012   954  4050  4050  46   3  34  17   0   0  -1  18.7  73
% 1520.0    7599  0.011   954  4056  4056  45   3  35  17   0   0  -1  18.7  73
% 1530.0    7494  0.011   954  4062  4062  44   3  35  17   0   0  -1  18.7  73
% 1540.0    7391  0.011   954  4068  4068  43   3  36  17   0   0  -1  18.7  73
% 1550.0    7290  0.011   954  4074  4074  43   3  36  18   0   0  -1  18.7  73
% 1560.0    7192  0.011   954  4080  4080  42   3  37  18   0   0  -1  18.7  73
% 1570.0    7096  0.011   954  4086  4086  41   3  37  18   0   0  -1  18.7  73
% 1580.0    7002  0.011   954  4092  4092  41   3  38  18   0   0  -1  18.7  73
% 1590.0    6910  0.010   954  4097  4097  40   3  38  19   0   0  -1  18.7  73
% 1600.0    6820  0.010   954  4103  4103  39   3  39  19   0   0  -1  18.7  73
% 1610.0    6733  0.010   954  4109  4109  39   3  39  19   0   0  -1  18.7  73
% 1620.0    6647  0.010   954  4115  4115  38   3  40  19   0   0  -1  18.7  73
% 1630.0    6563  0.010   954  4121  4121  37   3  40  20   0   0  -1  18.7  73
% 1640.0    6481  0.010   954  4127  4127  37   3  40  20   0   0  -1  18.7  73
% 1650.0    6401  0.010   954  4133  4133  36   3  41  20   0   0  -1  18.7  73
% 1660.0    6322  0.010   954  4139  4139  35   3  41  20   0   0  -1  18.7  73
% 1670.0    6246  0.009   954  4145  4145  35   3  42  21   0   0  -1  18.7  73
% 1680.0    6170  0.009   954  4151  4151  34   3  42  21   0   0  -1  18.7  73
% 1690.0    6097  0.009   954  4157  4157  34   3  43  21   0   0  -1  18.7  73
% 1700.0    6025  0.009   954  4163  4163  33   3  43  21   0   0  -1  18.7  73
% 1710.0    5954  0.009   954  4169  4169  32   3  43  22   0   0  -1  18.7  73
% 1720.0    5885  0.009   954  4175  4175  32   3  44  22   0   0  -1  18.7  73
% 1730.0    5817  0.009   954  4181  4181  31   3  44  22   0   0  -1  18.7  73
% 1740.0    5750  0.009   954  4187  4187  31   2  45  22   0   0  -1  18.7  73
% 1750.0    5685  0.009   954  4193  4193  30   2  45  22   0   0  -1  18.7  73
% 1760.0    5621  0.008   954  4199  4199  29   2  46  23   0   0  -1  18.7  73
% 1770.0    5559  0.008   954  4205  4205  29   2  46  23   0   0  -1  18.7  73
% 1780.0    5497  0.008   954  4211  4211  28   2  46  23   0   0  -1  18.7  73
% 1790.0    5437  0.008   954  4217  4217  28   2  47  23   0   0  -1  18.7  73
% 1800.0    5378  0.008   954  4223  4223  27   2  47  24   0   0  -1  18.7  73
% 1810.0    5320  0.008   954  4228  4228  27   2  47  24   0   0  -1  18.7  73
% 1820.0    5263  0.008   954  4234  4234  26   2  48  24   0   0  -1  18.7  73
% 1830.0    5208  0.008   954  4240  4240  25   2  48  24   0   0  -1  18.7  73
% 1840.0    5153  0.008   954  4246  4246  25   2  49  24   0   0  -1  18.7  73
% 1850.0    5099  0.008   954  4252  4252  24   2  49  25   0   0  -1  18.7  73
% 1860.0    5047  0.008   954  4258  4258  24   2  49  25   0   0  -1  18.7  73
% 1870.0    4995  0.008   954  4264  4264  23   2  50  25   0   0  -1  18.7  73
% 1880.0    4944  0.007   954  4270  4270  23   2  50  25   0   0  -1  18.7  73
% 1890.0    4894  0.007   954  4276  4276  22   2  50  25   0   0  -1  18.7  73
% 1900.0    4845  0.007   954  4282  4282  22   2  51  26   0   0  -1  18.7  73
% 1910.0    4797  0.007   954  4288  4288  21   2  51  26   0   0  -1  18.7  73
% 1920.0    4750  0.007   954  4294  4294  21   2  51  26   0   0  -1  18.7  73
% 1930.0    4703  0.007   954  4300  4300  20   2  52  26   0   0  -1  18.7  73
% 1940.0    4658  0.007   954  4306  4306  20   2  52  26   0   0  -1  18.7  73
% 1950.0    4613  0.007   954  4312  4312  20   2  52  27   0   0  -1  18.7  73
% 1960.0    4569  0.007   954  4318  4318  19   2  52  27   0   0  -1  18.7  73
% 1970.0    4525  0.007   954  4324  4324  19   2  53  27   0   0  -1  18.7  73
% 1980.0    4483  0.007   954  4330  4330  18   2  53  27   0   0  -1  18.7  73
% 1990.0    4441  0.007   954  4336  4336  18   2  53  27   0   0  -1  18.7  73
% 2000.0    4399  0.007   954  4342  4342  17   2  54  27   0   0  -1  18.7  73

  rtn = false;
  output = 1; % standard out

  IMZ = {' km ','GEOD','GEOD','yyyy',' mm ',' dd ','YEAR', ...
            'L.T.'};
  ITEXT = {'  H  ',' LATI', ...
            ' LONG',' YEAR','MONTH',' DAY ','DAYOF',' HOUR'};

  pna = {'NmF2','hmF2','NmF1','hmF1','NmE','hmE','NmD','hmD', ...
        'h05','B0','NVmin','hVtop','Tpeak','hTpek','T300','T400','T600', ...
        'T1400','T3000','T120','Ti450','hTeTi','sza','sundec','dip', ...
        'diplat','modip','Lati','Srise','Sset','season','Longi', ...
        'Rz12','cov','B1','M3000','TEC','TECtop','IG12','F1prb','F107d', ...
        'C1','daynr','vdrft','foF2r','F10781','foEr','sprd_F','MLAT', ...
        'MLON','Ap_t','Ap_d','invdip','MLT_i','CGMlat','CGMlon', ...
        'CGMmlt','AurBdy'};
  uni = {'m-3','km','m-3','km','m-3','km','m-3','km','km','km', ...
         'm-3','km','K','km',7*'K','km',6*'deg',2*'h',' ','deg',4*' ', ...
         'm-2','%',5*' ','m/s',4*' ',2*'deg',2*' ','deg','h',2*'deg', ...
         'h','deg'};
  timev = {'LT','UT'};
  coorv = {'geog','geom'};

  jfi = [IRI2012.FOF2_MODEL_SW,IRI2012.HMF2_MODEL_SW,IRI2012.FOF1_MODEL_SW, ...
         IRI2012.HMF1_MODEL_SW,IRI2012.FOE_MODEL_SW,IRI2012.HME_MODEL_SW];

  %    COMMON/const2/icalls,nmono,iyearo,idaynro,rzino,igino,ut0
  context = IRI2012();
        
  nummax=1000;
  oar = zeros(IRI2012.numAdditionalResults);
  jf = zeros(IRI2012.numSwitches,1);
  %jf = IRI2012.defaultIRIswitches( );
  for i=1:IRI2012.numAdditionalResults
    oar(i)=-1.0;
  end

  text = ' 0 40.0 -104.0 2012 0921 1 16.0333333 0.0 0 2000.0 1 0.0 2000.0 10.0 0';
  sc = textscan(text,'%f');
  sc = sc{1};
  % user input of IRI input parameters
  %
  %       fprintf(output,'jmag(=0/1,geog/geom),lati/deg,long/deg'
  jm = sc(1); xlat = sc(2);xlon=sc(3);
  %read(1, jm,xlat,xlon);
  iy = sc(4);imd = sc(5);iut=sc(6);hour = sc(7);
  fprintf(output,'year(yyyy): %d,mmdd(or -ddd): %d,iut(=0/1,LT/UT): %d,hour:  %f\n',iy,imd,iut,hour);
  %read(1, iy,imd,iut,hour);
  hx = sc(8);
  fprintf(output,'height/km: %f\n',hx);
  %read(1, hx);

  piktab = sc(9);
  fprintf(output,'output-option: %d\n',piktab);
  fprintf(output,'(enter 0 for standard table of IRI parameters)\n');
  fprintf(output,'(enter 1 for list of peak heights and densities)\n');
  fprintf(output,'(enter 2 for plasma frequencies, B0, M3000, valley, width and depth,)\n');
  fprintf(output,'(enter 3 for 6 parameters of your choice)\n');
  fprintf(output,'(enter 4 for D-region models at 60,65,..,110 km)\n');
  fprintf(output,'(enter 5 special test output)\n');
  %read(1, piktab);

  htec_max = sc(10);
  fprintf(output,'upper height [km] for TEC integration (0 for no TEC): %f\n',htec_max);
  %read(1, htec_max);

  IVAR = sc(11);
  fprintf(output,'variable? (1/2/../8 for height/lat/long/year/month/day/day of year/hour): %d\n',IVAR);
  %read(1, IVAR);
  vbeg = sc(12);vend = sc(13);vstp = sc(14);
  fprintf(output,'begin, end, and stepsize for the selected variable: %f %f %f\n', vbeg,vend,vstp);
  %read(1, vbeg,vend,vstp);

  jchoice = sc(15);
  fprintf(output,'Options: t(rue) or f(alse)\n');
  fprintf(output,'Enter 0 to use standard or 1 to enter your own: %d\n',jchoice);
  %read(1, jchoice);
  for i=1:IRI2012.numSwitches 
    jf(i)=true;
  end
  %jf(IRI2012.AUR_BOUND_MODEL_SW) = false;
  if(piktab == 4)
    jf(IRI2012.D_REGION_IRI1990_SW)=false;
  end
  if(jchoice == 0)
    %          jf(1)=false;      % f=no electron densities 
    %          jf(2)=false;      % f=no temperatures 
    %          jf(3)=false;      % f=no ion composition 
    jf(IRI2012.B0B1_Bil2000_SW)=false;      % t=B0table f=other models
    jf(IRI2012.FOF2_CCIR_SW)=false;      % t=CCIR  f=URSI foF2 model
    jf(IRI2012.Ni_DS_1995_SW)=false;      % t=DS95+DY85   f=RBV10+TTS03
    %          jf(7)=false;      % t=tops f10.7<188 f=unlimited
    %          jf(21)=false;     % f=ion drift not computed
    jf(IRI2012.Te_TOPS_Bil_1985_SW)=false;     % t=AEROS/ISIS f=TTS Te with PF10.7
    %          jf(24)=false;     % t=D-reg-IRI-1990 f=FT-2001
    %          jf(26)=false;    % f=STORM model turned off
    %          jf(28)=false;    % f=spread-F not computed
    jf(IRI2012.IRI01_TOPSIDE_SW)=false;     % t=old  f=New Topside options
    jf(IRI2012.IRI01_TOPSIDE_CORR_SW)=false;     % t=corr f=NeQuick topside
    %          jf(31)=false;     % t=B0ABT f=Gulyaeva
    %          jf(33)=false;     % t=auroral boundary on
    %          jf(35)=false;     % t=E-storm model on
    %          jf(36)=false;     % t=hmF2 w/out foF2_storm f=with
    %          jf(37)=false;     % t=topside w/out foF2_storm f=with
    %          jf(38)=false;     % t=WRITEs off in IRIFLIP f=on 
  else
    jf(IRI2012.Ne_COMPUTED_SW) = sc(16);
    jf(IRI2012.TeTi_COMPUTED_SW) = sc(17);
    jf(IRI2012.NeNi_COMPUTED_SW) = sc(18);
    
    fprintf(output,'Compute Ne, T, Ni? (enter: t,t,t  if you want all)\n');
    %read(1, jf(IRI2012.Ne_COMPUTED_SW),jf(IRI2012.TeTi_COMPUTED_SW),jf(IRI2012.NeNi_COMPUTED_SW));
    if(jf(IRI2012.Ne_COMPUTED_SW))
      jf(IRI2012.Ne_STANDARD_SW) = false;
      fprintf(output,'LAY version: t=standard ver., f=LAY version. {standard:t}\n');
      %read(1, jf(11));
      fprintf(output,'Ne Topside: t=IRI-2001/h0.5, f=new options {f}\n');
      read(1, jf(29));
      fprintf(output,'Ne Topside: t=IRI01_corrt, f=NeQuick/h0.5 {f}\n');
      read(1, jf(30));
      fprintf(output,'Ne Topside: t=F10.7<188, f=unlimited {t}\n');
      read(1, jf(7));
      fprintf(output,'foF2 model: t=CCIR, f=URSI-88 {f}\n');
      read(1, jf(5));
      fprintf(output,'foF2: t=with storm model, f=without {t}\n');
      read(1, jf(26));
      fprintf(output,'F2 peak density or foF2: t=model, f=user input {t}\n');
      read(1, jf(8));
      fprintf(output,'F2 peak height or M3000F2: t=model, f=user input {t}\n');
      read(1, jf(9));
      fprintf(output,'Auroral boundary model: t=on, f=off {f}\n');
      read(1, jf(33));
      fprintf(output,'Bottomside thickness B0: t=Bil-2000, f=other options {f}.\n');
      read(1, jf(4));
      fprintf(output,'Bottomside thickness B0: t=ABT-2009, f= Gul-1987 {t}.\n');
      read(1, jf(31));
      fprintf(output,'F1 peak density or foF1: t=model, f=user input {t}\n');
      read(1, jf(13));
      if(~jf(IRI2012.Ne_STANDARD_SW))
        fprintf(output,'F1 peak height: t=model, f=user input {t}\n');
        read(1, jf(14));
      end
      fprintf(output,'F1: t=with probability model, f=without   {t}\n');
      read(1, jf(19));
      fprintf(output,'F1: t=standard probability, f=with L condition {t}\n');
      read(1, jf(20));
      fprintf(output,'E peak density or foE: t=model, f=user input {t}\n');
      read(1, jf(15));
      fprintf(output,'E peak height: t=model, f=user input {t}\n');
      read(1, jf(16));
      fprintf(output,'E peak auroral storm model: t=on, f=off {f}\n');
      read(1, jf(35));
      fprintf(output,'D: t=IRI-1990, f= FT-2001 {t}\n');
      read(1, jf(24));
    end
    if(jf(IRI2012.TeTi_COMPUTED_SW))
      fprintf(output,'Te(Ne) model: t=not used, f=correlation is used. {t}\n');
      read(1, jf(10));
      fprintf(output,'Te: t=Bil-1985, f=TBT-2012 {f}\n');
      read(1, jf(23));
    end
    if(jf(IRI2012.NeNi_COMPUTED_SW))
      fprintf(output,'Ion comp. model: t=DS95/DY85, f=RBV10/TTS05 {f}\n' );
      read(1, jf(6));
      fprintf(output,'Ni: t=ion composition in %%, f=ion densities in cm-3 {t}\n');
      read(1, jf(22));
    end
    fprintf(output,'Equat. Vert. Ion Drift: t=computed, f=not computed {t}\n');
    read(1, jf(21));
    fprintf(output,'Spread-F probability: t=computed, f=not computed {t}\n');
    read(1, jf(28));
    fprintf(output,'Sunspot index: t=from file, f=user input.  {t}\n');
    read(1, jf(17));
    fprintf(output,'Ionospheric index: t=from file, f=user input. {t}\n');
    read(1, jf(27));
    fprintf(output,'F10.7D Index: t=from file, f=user input {t}\n');
    read(1, jf(25));
    fprintf(output,'F10.7_81 Index: t=from file, f=user input {t}\n');
    read(1, jf(IRI2012.F107_81_FILE_SW));
    fprintf(output,'UT/LT computation: t=no date change, f=ut_lt subroutine {t}\n');
    read(1, jf(IRI2012.IGRF_DIP_SW));
    fprintf(output,'Message output unit: t=(UNIT=6), f=(UNIT=12). {t}\n');
    read(1, jf(IRI2012.STANDARD_OUT_SW));
  end

  %       if(piktab > 3) jf(IRI2012.D_REGION_IRI1990_SW)=false
  % option to enter six additional parameters 
  %

  if(piktab == 3)
    fprintf(output,'6 Parameters of your choice (number:1-48)\n');
    for j=1:10
      fprintf(output,'%s ',pna{j});
    end
    for j=11:20
      fprintf(output,'%s ',pna{j});
    end
    for j=21:30
      fprintf(output,'%s ',pna{j});
    end
    for j=31:40
      fprintf(output,'%s ',pna{j});
    end
    for j=41:50
      fprintf(output,'%s ',pna{j});
    end
    for j=51:58
      fprintf(output,'%s ',pna{j});
    end
    fprintf(output,'or 0,0,0,0,0,0 for default:\n');
    fprintf(output,'      spread-F probability [48]\n');
    fprintf(output,'      equatorial vertical ion drift [44]\n');
    fprintf(output,'      foF2_storm/foF2_quiet [45]\n');
    fprintf(output,'      foE_storm/foE_quiet [47]\n' );
    fprintf(output,'      eqward auroral boundy CGM-Lat [58]\n');
    %        fprintf(output,'      solar zenith angle [23]\n');
    %        fprintf(output,'      modified dip latitude [27]\n');
    fprintf(output,'      Ap for current UT [51]\n' );
    pad1 = zeros(6,1);
    for j=1:6
      %read(1, pad1(j));
    end
    if(pad1(1) == 0)
      pad1(1)=IRI2012.SPREAD_F_PROB_OUT;     % spread-F probability
      jf(IRI2012.SPREAD_F_PROB_COMPUTED_SW)=true;
      pad1(2)=IRI2012.EQ_VERT_OUT;     % equatorial vertical ion drift
      jf(IRI2012.IONDRIFT_COMPUTED_SW)=true;
      pad1(3)=IRI2012.FOF2_STORM_OUT;     % fof2_storm/foF2_quiet
      jf(IRI2012.FOF2_STORM_MODEL_SW)=true;
      pad1(4)=IRI2012.FOE_STORM_OUT;     % foE_storm/foE_quiet
      jf(IRI2012.FOE_STORM_MODEL_SW)=true;           
      pad1(5)=IRI2012.CGM_LAT_AUR_BOUND_OUT;     % CGM_lat auroral boundary
      jf(IRI2012.AUR_BOUND_MODEL_SW)=true;            
      pad1(6)=IRI2012.AP_CURRENT_OUT;     % ap for current UT
    end
  end
       
  % option to enter measured values for NmF2, hmF2, NmF1, hmF1, NmE, hmE,
  % N(300), N(400), N(600) if available; 
  %
  fprintf(output,'\n');
  fprintf(output,'\n');
  fprintf(output,'\n');
  numstp=floor((vend-vbeg)/vstp)+1;
  if(IVAR == 1)
    numstp=1;
  end
  if(jf(IRI2012.Ne_COMPUTED_SW))
    if(~jf(IRI2012.FOF2_MODEL_SW) || ~jf(IRI2012.HMF2_MODEL_SW) || ...
       ~jf(IRI2012.FOF1_MODEL_SW) || ~jf(IRI2012.HMF1_MODEL_SW) || ...
       ~jf(IRI2012.FOE_MODEL_SW) || ~jf(IRI2012.HME_MODEL_SW))
      var=vbeg;
      i=1;
      while true
        if(~jf(IRI2012.FOF2_MODEL_SW))
          jf(IRI2012.FOF2_STORM_MODEL_SW)=false;    % storm model off, if user input
          oar(IRI2012.NMF2_IN_OUT,i) = 104.5;
          fprintf(output,'foF2/Mhz or NmF2/m-3 for %s %s %f\n',ITEXT{IVAR},'=',var);
          %read(1, oar(1,i));
          pname(1)='foF2/MHz';
          if(oar(IRI2012.NMF2_IN_OUT,i) > 30.)
            pname(1)='NmF2/m-3';
          end
        end
        if(~jf(IRI2012.HMF2_MODEL_SW))
          oar(IRI2012.HMF2_IN_OUT,i) = 104.5;
          fprintf(output,'hmF2/km or M3000F2 for %s %s %f\n',ITEXT{IVAR},'=',var);
          %read(1, oar(IRI2012.HMF2_IN_OUT,i));
          pname(2)='M(3000)F2';
          if(oar(IRI2012.HMF2_IN_OUT,i) > 50.)
            pname(2)='hmF2/km';
          end
        end
        if(~jf(IRI2012.FOF1_MODEL_SW))
          oar(IRI2012.NMF1_IN_OUT,i) = 104.5;
          fprintf(output,'foF1/MHz or NmF1/m-3 for %s %s %f\n',ITEXT{IVAR},'=',var);
          %read(1, oar(IRI2012.NMF1_IN_OUT,i));
          pname(3)='foF1/MHz';
          if(oar(IRI2012.NMF1_IN_OUT,i) > 30.)
            pname(3)='NmF1/m-3';
          end
        end
        if(~jf(IRI2012.HMF1_MODEL_SW))
          oar(IRI2012.HMF1_IN_OUT,i) = 104.5;
          fprintf(output,'hmF1/km for %s %s %f\n',ITEXT{IVAR},'=',var);
          %read(1, oar(IRI2012.HMF1_IN_OUT,i));
          pname(4)='hmF1/km';
        end
        if(~jf(IRI2012.FOE_MODEL_SW))
          oar(IRI2012.NME_IN_OUT,i) = 104.5;
          fprintf(output,'foE/MHz or NmE/m-3 for %s %s %f\n',ITEXT{IVAR},'=',var);
          %read(1, oar(IRI2012.NME_IN_OUT,i));
          pname(5)='foE/MHz';
          if(oar(IRI2012.NME_IN_OUT,i) > 30.)
            pname(5)='NmE/m-3';
          end
        end
        if(~jf(IRI2012.HME_MODEL_SW))
          oar(IRI2012.HME_IN_OUT,i) = 104.5;
          fprintf(output,'hmE/km for %s %s %f\n',ITEXT{IVAR},'=',var);
          %read(1, oar(IRI2012.HME_IN_OUT,i));
          pname(6)='hmE/km';
        end
        i=i+1;
        var=var+vstp;
        if(IVAR > 1 && var <= vend)
          continue;
        end
        break;
      end
    end
  end

  % option to enter Ne for Te-Ne relationship
  %
  if(jf(IRI2012.TeTi_COMPUTED_SW) && ~jf(IRI2012.Te_STANDARD_SW))
    var=vbeg;
    for i=1:numstp 
      oar(IRI2012.TE_MOD300_IN_OUT,i) = 4.15;
      oar(IRI2012.TE_MOD400_IN_OUT,i) = 4.16;
      fprintf(output,'Ne(300km),Ne(400km)/m-3 for %s %s %f %s\n',ITEXT{IVAR},'=',var,' [-1 if not]');
      %read(1, oar(IRI2012.TE_MOD300_IN_OUT,i),oar(IRI2012.TE_MOD400_IN_OUT,i));
      var=var+vstp;
    end
  end

  % option to enter F107D and/or PF107 
  %
  if(~jf(IRI2012.F107D_FILE_SW))
    f107d = 104.5;
    fprintf(output,'User input for F107D: %f\n',f107d);
    %read(1, f107d);
    for i=1:nummax
      oar(IRI2012.F107D_IN_OUT,i)=f107d;
    end
  end

  if(~jf(IRI2012.F107_81_FILE_SW))
    pf107d = 104.5;
    fprintf(output,'User input for PF107: %f\n',pf107d);
    %read(1, pf107d);
    for i=1:nummax
      oar(IRI2012.F107_81_IN_OUT,i)=pf107d;
    end
  end

  % option to enter Rz12 and/or IG12
  %
  if(~jf(IRI2012.RZ12_FILE_SW))
    oar(IRI2012.RZ12_IN_OUT,1) = 2.2;
    fprintf(output,'User input for Rz12: %f\n',oar(IRI2012.RZ12_IN_OUT,1));
    %read(1, oar(IRI2012.RZ12_IN_OUT,1));
    for i=2:nummax
      oar(IRI2012.RZ12_IN_OUT,i)=oar(IRI2012.RZ12_IN_OUT,1);
    end
  end

  if(~jf(IRI2012.IG12_FILE_SW))
    oar(IRI2012.IG12_IN_OUT,1) = 3.3;
    fprintf(output,'User input for IG12: %f\n',oar(IRI2012.IG12_IN_OUT,1));
    %read(1, oar(IRI2012.IG12_IN_OUT,1));
    for i=2:nummax
      oar(IRI2012.IG12_IN_OUT,i)=oar(IRI2012.IG12_IN_OUT,1);
    end
  end

  % end of user input
  %

  num1=floor((vend-vbeg)/vstp+1);
  numstp=abs(num1);
  if(numstp > nummax)
    numstp=nummax;
  end

  map='URSI';
  if(jf(IRI2012.FOF2_CCIR_SW))
    map='CCIR';
  end

  if(jf(IRI2012.B0B1_Bil2000_SW))
    bopt='BIl-2000';
  else
    if(jf(IRI2012.B0B1_ABT_2009_SW))
      bopt='ABT-2009';
    else
      bopt='Gul-1987';
    end
  end

  iopt='RBY10+TTS03';
  if(jf(IRI2012.Ni_DS_1995_SW))
    iopt='DS95 + DY85';
  end

  dopt='FT2001';
  if(jf(IRI2012.D_REGION_IRI1990_SW))
    dopt='IRI-95';
  end

  sopt='off';
  if(jf(IRI2012.FOF2_STORM_MODEL_SW))
    sopt='on ';
  end

  topt='TBT-2011';
  if(jf(IRI2012.Te_TOPS_Bil_1985_SW))
    topt='BIl-1985';
  end

  if(jf(IRI2012.F1PROB_MODEL_SW))
    f1opt='Scotto-97 no L';
    if(~jf(IRI2012.F1_STANDARD_SW))
      f1opt='Scotto-97 with L';
    end
  else
    f1opt='IRI-95';
  end

  hxx=hx;
  jmag=jm;
  mmdd=imd;

  % calling IRI subroutine
  % 
  phour=hour;
  [outf,oar,~] = context.IRI_WEB(jmag,jf,xlat,xlon,iy,mmdd,iut,hour, ...
          hxx,htec_max,IVAR,vbeg,vend,vstp,oar);
  [nr,nc] = size(outf);
  for irii=1:nr
    for irjj=1:nc
      if nargin > 0
        testCase.verifyFalse(isnan(outf(irii,irjj)));
      else
        %fprintf(output,'outf: %d %.10f\n', irii,outf(irii,1));
      end
    end
  end
  for irii=1:IRI2012.numAdditionalResults
    if nargin > 0
      testCase.verifyFalse(isnan(oar(irii,1)));
    else
      fprintf(output,'oar: %d %.10f\n', irii,oar(irii,1));
    end
  end
  % preparation of results page
  %
  fprintf(output,'\nyyyy/mmdd(or -ddd)/hh.h):%4d/%4d/%4.1f%s  %s Lat/Long/Alt=%5.1f/%6.1f/%6.1f/ B0 %.10f RSSN %.10f\n', ...
    iy,mmdd,phour,timev{iut+1},coorv{jmag+1},xlat,xlon,hxx,oar(IRI2012.TE_MOD300_IN_OUT,1),oar(IRI2012.TE_MOD400_IN_OUT,1));
  if(jf(IRI2012.Ne_COMPUTED_SW))
    if(jf(IRI2012.IRI01_TOPSIDE_SW))
      if(jf(IRI2012.IRI01_TOPSIDE_CORR_SW))
        fprintf(output,'IRI-2001 is used for topside Ne profile\n');
      else 
        fprintf(output,'h0.5 model is used for topside Ne profile\n');
      end
    else    
      if(jf(IRI2012.IRI01_TOPSIDE_CORR_SW))
        fprintf(output,'Corrected IRI01 is used for topside Ne profile\n');
      else 
        fprintf(output,'NeQuick is used for topside Ne profile\n');
      end
    end
    if(jf(IRI2012.FOF2_MODEL_SW))
      fprintf(output,'%s maps are used for the F2 peak density (NmF2)\n', map);
    end
    if(jf(IRI2012.HMF2_MODEL_SW))
      fprintf(output,'CCIR maps are used for the F2 peak height (hmF2)\n');
    end
    fprintf(output,'%s option is used for D-region\n', dopt);
    fprintf(output,'%s option is used for the bottomside thickness parameter B0\n', bopt);
    fprintf(output,'The foF2 STORM model is turned %s\n', sopt);
    fprintf(output,'%s option is used for the F1 occurrence probability\n', f1opt);
    numi=numstp;
    if(IVAR == 1)
      numi=1;
    end
    for j=1:length(jfi)
      ij=jfi(j);
      if(~jf(ij))
        fprintf(output,'%s provided by user:', pname(j));
        for i=1:numi
          fprintf(output,'%10.3f ', oar(j,i));
        end
        fprintf(output,'\n');
      end
    end
  end 

  if(jf(IRI2012.TeTi_COMPUTED_SW))
    fprintf(output,'%s option is used for the electron temperature\n', topt);
  end
  if(jf(IRI2012.NeNi_COMPUTED_SW))
    fprintf(output,'%s option is used for ion composition\n', iopt);
  end

  if(IVAR == 1)
    if(oar(IRI2012.NMF1_IN_OUT,1) < 1.)
      oar(IRI2012.HMF1_IN_OUT,1)=0.;
    end
    yp2=0;
    if(oar(IRI2012.NMF1_IN_OUT,1) > 0.0)
      yp2=oar(IRI2012.NMF1_IN_OUT,1)/1.e6;
    end
    if nargin > 0
      testCase.verifyEqual(oar(IRI2012.NMF2_IN_OUT,1)/1.E6,664629.25,'AbsTol',500.0);
      testCase.verifyEqual(yp2,0.0,'AbsTol',0.05);
      testCase.verifyEqual(oar(IRI2012.NME_IN_OUT,1)/1.E6,121468.59375,'AbsTol',500.0);
    else
      if oar(IRI2012.NMF2_IN_OUT,1)/1.E6 ~= 664629.25 || ...
          yp2 ~= 0.0 || ...
          oar(IRI2012.NME_IN_OUT,1)/1.E6 ~= 121468.59375
        fprintf(output,'Peak Densities/cm-3: NmF2=%9.5f   NmF1=%9.5f   NmE=%9.5f\n', ...
          oar(IRI2012.NMF2_IN_OUT,1)/1.E6,yp2,oar(IRI2012.NME_IN_OUT,1)/1.E6);
      end
    end
    if nargin > 0
      testCase.verifyEqual(oar(IRI2012.HMF2_IN_OUT,1),259.52805,'AbsTol',0.005);
      testCase.verifyEqual(oar(IRI2012.HMF1_IN_OUT,1),0.00,'AbsTol',0.005);
      testCase.verifyEqual(oar(IRI2012.HME_IN_OUT,1),110.00,'AbsTol',0.005);
    else
      if oar(IRI2012.HMF2_IN_OUT,1) ~= 259.52805 || ...
         oar(IRI2012.HMF1_IN_OUT,1) ~= 0.00 || ...
         oar(IRI2012.HME_IN_OUT,1) ~= 110.00
        fprintf(output,'Peak Heights/km:     hmF2=%9.5f   hmF1=%9.5f   hmE=%9.5f\n', ...
          oar(IRI2012.HMF2_IN_OUT,1),oar(IRI2012.HMF1_IN_OUT,1),oar(IRI2012.HME_IN_OUT,1));
      end
    end
  else
    fprintf(output,' Solar and magnetic parameter for the 1st profile point:\n');
  end

  if nargin > 0
    testCase.verifyEqual(oar(IRI2012.SOL_ZENITH_ANG_OUT,1),55.1,'AbsTol',0.05);
    testCase.verifyEqual(oar(IRI2012.DIP_OUT,1),66.76,'AbsTol',0.005);
    testCase.verifyEqual(oar(IRI2012.MODIP_LAT_OUT,1),53.09,'AbsTol',0.005);
  else
    if oar(IRI2012.SOL_ZENITH_ANG_OUT,1) ~= 55.1 || ...
       oar(IRI2012.DIP_OUT,1) ~= 66.76 || ...
       oar(IRI2012.MODIP_LAT_OUT,1) ~= 53.09
      fprintf(output,'Solar Zenith Angle/degree                            %6.1f\n', oar(IRI2012.SOL_ZENITH_ANG_OUT,1));
      fprintf(output,' Dip (Magnetic Inclination)/degree                    %6.2f\n',oar(IRI2012.DIP_OUT,1));
      fprintf(output,' Modip (Modified Dip)/degree                          %6.2f\n',oar(IRI2012.MODIP_LAT_OUT,1));
    end
  end

  if(~jf(IRI2012.RZ12_FILE_SW))
    fprintf(output,'Solar Sunspot Number (12-months running mean) Rz12   %5.1f {user provided input}\n', oar(IRI2012.RZ12_IN_OUT,1));
  else
    if nargin > 0
      testCase.verifyEqual(oar(IRI2012.RZ12_IN_OUT,1),66.3,'AbsTol',0.05);
    else
      if oar(IRI2012.RZ12_IN_OUT,1) ~= 66.3
        fprintf(output,'Solar Sunspot Number (12-months running mean) Rz12    %5.1f\n', oar(IRI2012.RZ12_IN_OUT,1));
      end
    end
  end

  if(~jf(IRI2012.IG12_FILE_SW))
    fprintf(output,'Ionospheric-Effective Solar Index IG12                %5.1f {user provided input}\n', oar(IRI2012.IG12_IN_OUT,1));
  else
    if nargin > 0
      testCase.verifyEqual(oar(IRI2012.IG12_IN_OUT,1),69.3,'AbsTol',0.05);
    else
      if oar(IRI2012.IG12_IN_OUT,1) ~= 69.3
        fprintf(output,'Ionospheric-Effective Solar Index IG12                %5.1f\n', oar(IRI2012.IG12_IN_OUT,1));
      end
    end
  end


  if(htec_max > 50.0)
    fprintf(output,'TEC [1.E16 m-2] is obtained by numerical integration in 1km steps\n');
    fprintf(output,'  from 50 to %6.1f km.  t is the percentage of TEC above the F peak.\n', htec_max);
  end

  %
  % table head .......................................................
  %

  %agnr=7;          %output unit number
  xtex=IMZ(IVAR);
  if(jmag > 0 && (IVAR == 2 || IVAR == 3))
    xtex='GEOM';
  end
  if(iut > 0 && IVAR == 8)
    xtex='U.T.';
  end

  if(piktab == 4)
    fprintf(output,'- h         D-REGION ELECTRON DENSITY IN CM-3 km                  DRS-95: Stratos Warming/Winter Anomaly     IRI-07    FIRI  SW/WA=0/0  0.5/0   1/0    0/0.5    0/1\n');
  end
  if(piktab == 3)
    fprintf(output,'-  %s', ITEXT{IVAR});
    for j=1:6
      fprintf(output,'%s   ',pna{pad1(j)});
    end
    fprintf(output,'%s',xtex);
    for j=1:6
      fprintf(output,'%s', uni{pad1(j)});
    end
    fprintf(output,'\n');
  end
  if(piktab == 2)
    fprintf(output,'-  %s   M3000   B0   B1   E-VALLEY       PLASMA FREQUENCIES / MHz\n', ITEXT{IVAR});
    fprintf(output,'   %s           km       W/km  Depth     foF2   foF1   foE   foD\n', xtex{1});
  end
  if(piktab == 1)
    fprintf(output,'-  %s      PEAK ALTITUDES IN KM        PEAK DENSITIES IN cm-3  TEC top/%%\n', ITEXT{IVAR});
    fprintf(output,'   %s    hmF2  hmF1   hmE   hmD      NmF2   NmF1    NmE    NmD  1E16m-2\n', xtex{1});
  end
  if(piktab == 0)
    fprintf(output,'- %s ELECTRON DENSITY   TEMPERATURES         ION PERCENTAGES/%%     1E16m-2\n', ITEXT{IVAR});
    fprintf(output,'   %s Ne/cm-3 dNe/cm-3/km Ne/NmF2  Tn/K dTn /K/km  Ti/K dTi/K/km Te/K dTe/K/km O+  N+  H+ He+ O2+ NO+ Clust TEC t/%%\n', xtex{1});
  end

  %
  % output: D-region piktab=4
  %
  % D-REGION ELECTRON DENSITY IN CM-3: 
  %    IRI-07    FIRI  Danilov:SW/WA=0/0  0.5/0   1/0    0/0.5    0/1 
  %                    DRS-95: Stratos Warming/Winter Anomaly
  %
  jdprof = zeros(77,1);
  if(piktab == 4)
    for lix=1:77 
      jdprof(lix)=-1;
      dichte=outf(IRI2012.D_DENS_OUT,lix);
      if(dichte > 0.)
        jdprof(lix)=floor(dichte/1.e6+0.5);
      end
    end
    for lix=1:IRI2012.numDregHeights
      ihtemp=55+lix*5;
      fprintf(output,'%3d %8d %8d %8d %8d %8d %8d %8d\n', ihtemp,jdprof(lix),jdprof(lix+11), ...
            jdprof(lix+22),jdprof(lix+33),jdprof(lix+44), ...
            jdprof(lix+55),jdprof(lix+66));
    end
    return;
  end
    
  XCOR=vbeg;

  for li=1:numstp

    %
    % output: peak densities and altitudes piktab=1
    %

    if(piktab == 1)
      if(oar(IRI2012.NMF1_IN_OUT,li) < 1.)
        oar(IRI2012.HMF1_IN_OUT,li)=0.;
      end
      iyp1=floor(oar(IRI2012.NMF2_IN_OUT,li)/1.e6+.5);
      iyp2=0;
      if(oar(IRI2012.NMF1_IN_OUT,li) > 0.0)
        iyp2=floor(oar(IRI2012.NMF1_IN_OUT,li)/1.e6+.5);
      end
      iyp3=floor(oar(IRI2012.NME_IN_OUT,li)/1.e6+.5);
      iyp4=floor(oar(IRI2012.NMD_OUT,li)/1.e6+.5);
      tec=oar(IRI2012.TEC_OUT,li);
      if(tec > 0.0)
        tec=tec/1.e16;
        itopp=floor(oar(IRI2012.TEC_TOP_OUT,li)+.5);
      else
        tec=-1.0;
        itopp=-1;
      end
      fprintf(output,'%7.1f  %6.1f  %6.1f  %6.1f  %6.1f %9d %7d %7d %7d %6.2f %4d\n', ...
        XCOR,oar(IRI2012.HMF2_IN_OUT,li),oar(IRI2012.HMF1_IN_OUT,li), ...
        oar(IRI2012.HME_IN_OUT,li),oar(IRI2012.HMD_OUT,li), ...
        iyp1,iyp2,iyp3,iyp4,tec,itopp);
      continue;
    end
%
% output: plasma frequencies and profile parameters  piktab=2
%

    if(piktab == 2)
      if(oar(IRI2012.NMF1_IN_OUT,li) < 1.)
        oar(IRI2012.HMF1_IN_OUT,li)=0.;
      end
      fyp1=sqrt(oar(IRI2012.NMF2_IN_OUT,li)/IRI2012.resonance);
      fyp2=0;
      if(oar(IRI2012.NMF1_IN_OUT,li) > 0.0)
        fyp2=sqrt(oar(IRI2012.NMF1_IN_OUT,li)/IRI2012.resonance);
      end
      fyp3=sqrt(oar(IRI2012.NME_IN_OUT,li)/IRI2012.resonance);
      fyp4=sqrt(oar(IRI2012.NMD_OUT,li)/IRI2012.resonance);
      tec=oar(IRI2012.TEC_OUT,li);
      if(tec > 0.0)
        %tec=tec/1.e16;
        %itopp=floor(oar(IRI2012.TEC_TOP_OUT,li)+.5);
      else
        %tec=-1.0;
        %itopp=-1;
      end
      wvalley=oar(IRI2012.VALLEY_TOP_OUT,li)-oar(6,li);
      dvalley=0.0;
      if(oar(IRI2012.NME_IN_OUT,li) > 0.0)
        dvalley=oar(IRI2012.VALLEY_BASE_OUT,li)/oar(IRI2012.NME_IN_OUT,li);
      end
      fprintf(output,'7.1f  %6.4f %6.1f %4.1f %6.1f %8.4f %7.3f %7.3f %7.3f %7.3f\n', ...
        XCOR,oar(IRI2012.M3000_F2_OUT,li),oar(IRI2012.B0_OUT,li),oar(IRI2012.B1_OUT,li), ...
        wvalley,dvalley,fyp1,fyp2,fyp3,fyp4);
      continue;
    end
    %
    % output: 6 parameters of your choice    piktab=3
    %

    if(piktab == 3)
      %        if(pad1 == 45 && oar(pad1,li) <= 0.0) oar(pad1,li)=-1.
      %        if(pad2 == 45 && oar(pad2,li) <= 0.0) oar(pad2,li)=-1.
      %        if(pad3 == 45 && oar(pad3,li) <= 0.0) oar(pad3,li)=-1.
      fprintf(output,'%7.1f %9.2f %9.2f %9.2f %9.2f %9.2f %9.2f\n', ...
            XCOR,oar(pad1(1),li),oar(pad1(2),li), ...
            oar(pad1(3),li),oar(pad1(4),li),oar(pad1(5),li), ...
            oar(pad1(6),li));
      continue;
    end
    %
    % output: special for test purposes    piktab=5
    %

    if(piktab == 5)
      % ----------- B0, B1 ----------------
      %        WRITE(8,4919) XCOR,oar(10,li),oar(35,li)
      %4919    FORMAT(F7.1,2X,F6.2,2X,F5.3)
      % ----------- Te ----------------
      %        WRITE(8,4919) XCOR,outf(2,li),outf(3,li),outf(4,li)
      %4919    FORMAT(F7.1,2X,F6.1,2X,F6.1,2X,F6.1)
      % ----------- Ne, TEC ----------------
      %        WRITE(8,4919) XCOR,outf(1,li),oar(37,li)
      %4919    FORMAT(F7.1,2X,E12.5,2X,E12.5)
      % ----------- Ni ----------------
      %        type*,XCOR,outf(1,li),outf(5,li),outf(6,li),
      %     &   outf(7,li),outf(8,li),outf(9,li),outf(10,li),outf(11,li)
      %        WRITE(8,4919) XCOR,outf(1,li),outf(5,li),outf(6,li),
      %     &   outf(7,li),outf(8,li),outf(9,li),outf(10,li),outf(11,li)
      %4919    FORMAT(F7.1,2X,E12.5,7F10.4)
      % ----------- hmF2 ----------------
      %        type*,XCOR,oar(26,li),oar(2,li),oar(36,li),
      %     &   sqrt(oar(1,li)/oar(5,li)),oar(33,li),oar(39,li)
      %        WRITE(8,4919) XCOR,outf(1,li),outf(5,li),outf(6,li),
      %     &   outf(7,li),outf(8,li),outf(9,li),outf(10,li),outf(11,li)
      %4919    FORMAT(F7.1,2X,E12.5,7F10.4)
      % ----------- auroral boundary ----------------
      %        print *,XCOR,oar(55,li),oar(56,li),oar(54,li),
      %     &     oar(57,li),oar(54,li)-oar(57,li),oar(58,li)
      %        WRITE(8,4919) XCOR,outf(1,li),outf(5,li),outf(6,li),
      %     &   outf(7,li),outf(8,li),outf(9,li),outf(10,li),outf(11,li)
      %4919    FORMAT(F7.1,2X,E12.5,7F10.4)

      continue;
    end
    %
    % output: standard
    %
    if(IVAR == 1)
      oar(IRI2012.NMF2_IN_OUT,li)=oar(IRI2012.NMF2_IN_OUT,1);
      oar(IRI2012.TEC_OUT,li)=oar(IRI2012.TEC_OUT,1);
      oar(IRI2012.TEC_TOP_OUT,li)=oar(IRI2012.TEC_TOP_OUT,1);
    end
    jne=floor(outf(IRI2012.EL_DENS_OUT,li)/1.e6+.5);
    djne=outf(IRI2012.EL_GRAD_OUT,li)/1.e6;
    xner=outf(IRI2012.EL_DENS_OUT,li)/oar(IRI2012.NMF2_IN_OUT,li);
    jtn=floor(outf(IRI2012.NT_TEMP_OUT,li)+.5);
    jti=floor(outf(IRI2012.IO_TEMP_OUT,li)+.5);
    jte=floor(outf(IRI2012.EL_TEMP_OUT,li)+.5);
    djtn=outf(IRI2012.NT_GRAD_OUT,li);
    djti=outf(IRI2012.IO_GRAD_OUT,li);
    djte=outf(IRI2012.ET_GRAD_OUT,li);
    jio=floor(outf(IRI2012.O_DENS_OUT,li)+.5);
    jih=floor(outf(IRI2012.H_DENS_OUT,li)+.5);
    jihe=floor(outf(IRI2012.HE_DENS_OUT,li)+.5);
    jino=floor(outf(IRI2012.O2_DENS_OUT,li)+.5);
    jio2=floor(outf(IRI2012.NO_DENS_OUT,li)+.5);
    jicl=floor(outf(IRI2012.CL_DENS_OUT,li)+.5);
    jin=floor(outf(IRI2012.N_DENS_OUT,li)+.5);
    %        fprintf(output,'O+,N+,O2+,NO+=',outf(5,li),
    %     &    outf(11,li),outf(9,li),outf(8,li)
    if(outf(IRI2012.EL_DENS_OUT,li) < 0)
      jne=-1;
    end
    if(outf(IRI2012.EL_DENS_OUT,li) < 0)
      xner=-1.;
    end
    if(outf(IRI2012.NT_TEMP_OUT,li) < 0)
      jtn=-1;
    end
    if(outf(IRI2012.IO_TEMP_OUT,li) < 0)
      jti=-1;
    end
    if(outf(IRI2012.EL_TEMP_OUT,li) < 0)
      jte=-1;
    end
    if(outf(IRI2012.O_DENS_OUT,li) < 0)
      jio=-1;
    end
    if(outf(IRI2012.H_DENS_OUT,li) < 0)
      jih=-1;
    end
    if(outf(IRI2012.HE_DENS_OUT,li) < 0)
      jihe=-1;
    end
    if(outf(IRI2012.O2_DENS_OUT,li) < 0)
      jino=-1;
    end
    if(outf(IRI2012.NO_DENS_OUT,li) < 0)
      jio2=-1;
    end
    if(outf(IRI2012.CL_DENS_OUT,li) < 0)
      jicl=-1;
    end
    if(outf(IRI2012.N_DENS_OUT,li) < 0)
      jin=-1;
    end
    tec=oar(IRI2012.TEC_OUT,li);
    if(tec > 0.0)
      tec=tec/1.e16;
      itopp=floor(oar(IRI2012.TEC_TOP_OUT,li)+.5);
    else
      tec=-1.0;
      itopp=-1;
    end
    %        print *, XCOR,jne,xner,jtn,jti,jte,jio,jin,
    %     &        jih,jihe,jino,jio2,jicl,tec,itopp
    fprintf(output,'%6.1f %8d %6.3f %6.3f %6d %6.3f %6d %6.3f %6d %6.3f %4d %4d %4d %4d %4d %4d %4d %6.1f %4d\n', ...
      XCOR,jne,djne,xner,jtn,djtn,jti,djti,jte,djte,jio,jin,jih,jihe,jino,jio2,jicl,tec,itopp);
    XCOR = XCOR + vstp;
  end
  rtn = true;
end
  
