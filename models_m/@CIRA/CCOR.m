function [ CCOR,CCORdZ,CCORdR,CCORdH1,CCORdZH ] = CCOR( ALT, R,H1,ZH )
%CCOR CHEMISTRY/DISSOCIATION CORRECTION FOR MSIS MODELS
%      FUNCTION  CCOR(ALT, R,H1,ZH)
%-----------------------------------------------------------------------
%        CHEMISTRY/DISSOCIATION CORRECTION FOR MSIS MODELS
%        ALT - altitude (km)
%        R - target ratio
%        H1 - transition scale length (km)
%        ZH - altitude of 1/2 R (km)
%-----------------------------------------------------------------------

  if H1 == 0
    if ALT > ZH
      CCOR=0.0;
      CCORdZ=0.0;
      CCORdR=0.0;
      CCORdH1=0.0;
      CCORdZH=0.0;
    else
      CCOR=R;
      CCORdZ=0.0;
      CCORdR=1.0;
      CCORdH1=0.0;
      CCORdZH=0.0;
    end
  else
    E=(ALT-ZH)/H1;
    if E > CIRA.ARGMAX % limit as E goes to infinity
      CCOR=0.0; % CCOR < R*3.975e-31
      CCORdZ=0.0;
      CCORdR=0.0;
      %CCORdH1=(R/H1)*exp(E)*E/(1.+exp(E))^2;
      %CCORdH1=(R/(ALT-ZH))*exp(E)*E*E/(1.+exp(E))^2;
      CCORdH1=0.0;
      CCORdZH=0.0;
    elseif E < -CIRA.ARGMAX % limit as E goes to -infinity
      CCOR=R; % CCOR > R - R*3.975e-31
      CCORdZ=0.0;
      CCORdR=1.0;
      %CCORdH1=(R/H1)*E/(4*cosh(E/2)^2);
      CCORdH1=0.0;
      CCORdZH=0.0;
    else
      EX=exp(E);
      EXdZ=EX/H1;
      EXdH1=-EX*E/H1;
      EXdZH=-EX/H1;
      CCOR=R/(1.+EX);
      CCORdZ=-CCOR*EXdZ/(1.+EX);
      CCORdR=1.0/(1.+EX);
      CCORdH1=-CCOR*EXdH1/(1.+EX);
      CCORdZH=-CCOR*EXdZH/(1.+EX);
    end
  end
  CCOR=exp(CCOR);
  CCORdZ=CCOR*CCORdZ;
  CCORdR=CCOR*CCORdR;
  CCORdH1=CCOR*CCORdH1;
  CCORdZH=CCOR*CCORdZH;

end

