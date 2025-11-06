analysis_lpf.do=1;
analysis_lpf.lib='resampler';
if name_site=='V'
    analysis_lpf.par=[20000 128*5*2 15];
else
    analysis_lpf.par=[20000 128*5 15];
end
analysis_lpf.raw=[];

analysis_lpf(2).par=load([path_expr 'leo_u.par']);
analysis_lpf(2).lib='plwin';
analysis_lpf(2).raw=136+row(1333*ones(1190,1)*((1:5*128)-1)+(1:1190)'*ones(1,5*128));
analysis_lpf(2).data=14;
analysis_altit=80+(0:3:700^.7).^1.4;
a_satch.cut=1;

analysis_lpf(3).lib='pow';
analysis_lpf(3).par=[7,2]; %vec_len, res_mult
analysis_lpf(3).raw=1326+row(1333*ones(7,1)*((1:5*128)-1)+(1:7)'*ones(1,5*128));
analysis_lpf(3).data=0;
analysis_lpf(3).lpf=[0 128*5-1];
analysis_lpf(3).nrep=5*128;

if name_site=='V'
    analysis_lpf(4).par=load([path_expr 'leo_u.par']);
    analysis_lpf(4).lib='plwin';
    analysis_lpf(4).raw=1333*128*5+136+row(1333*ones(1190,1)*((1:5*128)-1)+(1:1190)'*ones(1,5*128));
    analysis_lpf(4).data=14+(580+1)*2+580*128/2+1190 +14;

    analysis_lpf(5).lib='pow';
    analysis_lpf(5).par=[7,2]; %vec_len, res_mult
    analysis_lpf(5).raw=1333*128*5+1326+row(1333*ones(7,1)*((1:5*128)-1)+(1:7)'*ones(1,5*128));
    analysis_lpf(5).data=14+(580+1)*2+580*128/2+1190;
    analysis_lpf(5).lpf=[0 128*5-1];
    analysis_lpf(5).nrep=5*128;
end
