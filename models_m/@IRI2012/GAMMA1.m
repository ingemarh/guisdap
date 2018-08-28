function [ GAMMA1 ] = GAMMA1( SMODIP,SLAT,SLONG,HOUR,IHARM,NQ,K1,M,MM,M3,SFE )
%GAMMA1 CALCULATES GAMMA1=FOF2 OR M3000
%
%        REAL FUNCTION GAMMA1(SMODIP,SLAT,SLONG,HOUR,
%     &                          IHARM,NQ,K1,M,MM,M3,SFE)      
%---------------------------------------------------------------
% CALCULATES GAMMA1=FOF2 OR M3000 USING CCIR NUMERICAL MAP                      
% COEFFICIENTS SFE(M3) FOR MODIFIED DIP LATITUDE (SMODIP/DEG)
% GEOGRAPHIC LATITUDE (SLAT/DEG) AND LONGITUDE (SLONG/DEG)  
% AND UNIVERSIAL TIME (HOUR/DECIMAL HOURS). IHARM IS THE MAXIMUM
% NUMBER OF HARMONICS USED FOR DESCRIBING DIURNAL VARIATION.
% NQ(K1) IS AN INTEGER ARRAY GIVING THE HIGHEST DEGREES IN 
% LATITUDE FOR EACH LONGITUDE HARMONIC WHERE K1 GIVES THE NUMBER 
% OF LONGITUDE HARMONICS. M IS THE NUMBER OF COEFFICIENTS FOR 
% DESCRIBING VARIATIONS WITH SMODIP, SLAT, AND SLONG. MM IS THE
% NUMBER OF COEFFICIENTS FOR THE FOURIER TIME SERIES DESCRIBING
% VARIATIONS WITH UT.
% M=1+NQ(1)+2*[NQ(2)+1]+2*[NQ(3)+1]+... , MM=2*IHARM+1, M3=M*MM  
% SHEIKH,4.3.77.      
%---------------------------------------------------------------

%      REAL*8 C(12),S(12),COEF(100),SUM             
%      DIMENSION NQ(K1),XSINX(13),SFE(M3)           
%      COMMON/CONST/UMR
  C = zeros(IHARM,1,IRI2012.float_t);
  S = zeros(IHARM,1,IRI2012.float_t);
  COEF = zeros(100,1,IRI2012.float_t);
  XSINX = zeros(13,1,IRI2012.float_t);
  HOU=(15.0*HOUR-180.0)*IRI2012.UMR;
  S(1)=sin(HOU);
  C(1)=cos(HOU);

  for I=2:IHARM                             
    C(I)=C(1)*C(I-1)-S(1)*S(I-1);
    S(I)=C(1)*S(I-1)+S(1)*C(I-1);
  end

  for I=1:M    
    MI=(I-1)*MM;
    COEF(I)=SFE(MI+1);
    for J=1:IHARM
      COEF(I)=COEF(I)+SFE(MI+2*J)*S(J)+SFE(MI+2*J+1)*C(J);
    end
  end

  SUM=COEF(1);
  SS=sin(SMODIP*IRI2012.UMR);
  S3=SS;
  XSINX(1)=1.0;
  INDEX=NQ(1);

  for J=1:INDEX                             
    SUM=SUM+COEF(1+J)*SS;
    XSINX(J+1)=SS;
    SS=SS*S3;
  end

  XSINX(NQ(1)+2)=SS;
  NP=NQ(1)+1;
  SS=cos(SLAT*IRI2012.UMR);
  S3=SS;

  for J=2:K1   
    S0=SLONG*(J-1.)*IRI2012.UMR;
    S1=cos(S0);
    S2=sin(S0);
    INDEX=NQ(J)+1;
    for L=1:INDEX                             
      NP=NP+1;
      SUM=SUM+COEF(NP)*XSINX(L)*SS*S1;
      NP=NP+1;
      SUM=SUM+COEF(NP)*XSINX(L)*SS*S2;
    end
    SS=SS*S3;
  end

  GAMMA1=SUM;

end

