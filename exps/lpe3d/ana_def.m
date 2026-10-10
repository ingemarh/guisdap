lpinit(1,:)={'lpe3d','v',1,[88 1012],[1120 6820],[7075 9400],[9545 9980],10000,15,'b30d225.fir',224,29,0};
clear analysis_maxwidth
analysis_altit=logspace(log10(100),log10(1000),40);
a_satch.cut=1;
a_satch.skip=10;
a_satch.clutter=15;
