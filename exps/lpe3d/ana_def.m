if name_site=='V'
 lpinit(1,:)={'lpe3d','V',1,[88 1012],[1120 6820],[7075 9400],[9545 9980],10000,15,'b30d225.fir',224,29,0};
 clear analysis_maxwidth
 analysis_altit=logspace(log10(100),log10(1000),40);
 a_satch.cut=1;
 a_satch.skip=10;
 a_satch.clutter=15;
else
 lpinit(1,:)={'lpe3d','I',1,[88 1012],[1000 4000],[8000 9000],[],10000,10,10,224,29,-1000};
 analysis_lpf.par=[1 1 diff(lpinit{expver,5})/lpinit{expver,9} 0 0 0 0 lpinit{expver,12}+1];
 analysis_lpf(1).data=0;
 analysis_lpf(1).raw=0;
 analysis_lpf(1).skip=lpinit{expver,5}(1)/lpinit{expver,9};
 analysis_lpf(2).par=analysis_lpf(1).par;
 analysis_lpf(2).par(3)=diff(lpinit{expver,6})/lpinit{expver,9};
 analysis_lpf(2).data=[];
 analysis_lpf(2).raw=analysis_lpf(1).par(3);
 analysis_lpf(2).skip=lpinit{expver,6}(1)/lpinit{expver,9};
 analysis_lpf(3).par=[1 1 lpinit{expver,8}/lpinit{expver,9} 0 0 0 0 1];
 analysis_lpf(3).raw=analysis_lpf(2).raw+analysis_lpf(2).par(3);
 analysis_lpf(3).data=[];
 analysis_lpf(3).skip=0;
 [analysis_lpf.lib]=deal('clutter');
 [analysis_lpf.do]=deal(1);
 [analysis_lpf.loop]=deal(round(1e6/lpinit{expver,8})); % ~1s dumps
 analysis_Offsetppd=-lpinit{expver,13};
 analysis_code=1;
 a_phasepush=634.8-21; %offset-VHFphasepush
end
