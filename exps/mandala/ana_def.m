% Analysis defaults
if name_site=='V'
  fit_altitude([2 3 5],1)=[120;700;100];
  a_satch.clutter=90;
  if fix(expver)==4
    a_satch.clutter=250;

    analysis_lpf.do=1;
    analysis_lpf(1).lib='store';
    analysis_lpf(1).par=20;
    analysis_lpf(1).data=101252;
    analysis_lpf(1).nrep=1;

    analysis_lpf(2).par=load([path_expr 'mandala_vclu.par']);
    analysis_lpf(2).lib='clutter';
    analysis_lpf(2).raw=32768;
    analysis_lpf(2).data=0;
    
    analysis_lpf(3).par=load([path_expr 'mandala_va.par']);
    analysis_lpf(3).lib='plwin';
    analysis_lpf(3).raw=32768;
    analysis_lpf(3).data=20;
    
    analysis_lpf(4).par=load([path_expr 'mandala_vb.par']);
    analysis_lpf(4).lib='plwin';
    analysis_lpf(4).raw=32768;
    analysis_lpf(4).data=20+65482;
    
    analysis_lpf(5).par=load([path_expr 'mandala_vc.par']);
    analysis_lpf(5).lib='plwin';
    analysis_lpf(5).raw=32768;
    analysis_lpf(5).data=20+65482+64540;
    
    analysis_lpf(6).lib='restore';
    analysis_lpf(6).data=20+65482+64540*2;
    analysis_lpf(6).nrep=1;
    
    analysis_lpf(7)=analysis_lpf(2);
    analysis_lpf(7).raw=32768*2+3014400;
    
    analysis_lpf(8)=analysis_lpf(3);
    analysis_lpf(8).raw=32768*2+3014400;
    analysis_lpf(8).data=20*2+65482+64540*2;
    
    analysis_lpf(9)=analysis_lpf(4);
    analysis_lpf(9).raw=32768*2+3014400;
    analysis_lpf(9).data=20*2+65482*2+64540*2;
    
    analysis_lpf(10)=analysis_lpf(5);
    analysis_lpf(10).raw=32768*2+3014400;
    analysis_lpf(10).data=20*2+65482*2+64540*3;
  end
end

if (name_site=='T' | name_site=='L' | name_site=='V')
 a_satch.repair=63; 
 a_satch.skip=1;
 analysis_range=[20:69,logspace(log10(70),log10(800),140)];
 analysis_maxwidth=ones(size(analysis_range));
end
