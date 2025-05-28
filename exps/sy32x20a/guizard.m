if d_parbl(60)~=666
 rdiff=d_parbl(60)-666; %diff from init
 lpg_h=lpg_h+rdiff;
end
if strcmp(iono_model,'bafim')
fit_altitude(1:6,1:4)=[  0 Inf 0.1  2e11 ;
                        90 Inf 0.3    10 ;
                       113 Inf 0.3  0.05 ;
                         0   0   0     0 ;
                        80 Inf 0.2   2.5 ;
                       140 350 0.2 0.003 ];
fit_altitude(1:6,1:4)=[ 0 Inf 0.01  3e12 ;
                        80 Inf 0.01    100 ;
                       110 Inf 0.01  1 ;
                         0   0   0     0 ;
                        80 Inf 0.01   50 ;
                       0 0 0.1 1 ];
 a_phasepush=0;
 a_satch.cut=0;
 analysis_altit(find(analysis_altit>500))=[];
else
 fit_altitude(6,1:2)=[360 Inf]; % Fit for H+
end
