function [ CCOR2, CCOR2dZ, CCOR2dR ] = CCOR2( ALT,R,H1,ZH,H2 )
%CCOR2 O&O2 CHEMISTRY/DISSOCIATION CORRECTION FOR MSIS MODELS
%      FUNCTION  CCOR2(ALT, R,H1,ZH,H2)
%-----------------------------------------------------------------------
%       O&O2 CHEMISTRY/DISSOCIATION CORRECTION FOR MSIS MODELS
%        ALT - altitude (km)
%        R - target ratio
%        H1 - transition scale length (km)
%        ZH - altitude of 1/2 R (km)
%        H2 - transition scale length (km)
%-----------------------------------------------------------------------
  if H1 == 0 || H2 == 0
    if ALT > ZH
      CCOR2=0.; % CCOR2 < R*2.0*3.975e-31
      CCOR2dZ=0.0;
      CCOR2dR=0.0;
    elseif H1 == 0 && H2 == 0
      CCOR2=R; % CCOR2 > R - R*3.975e-31
      CCOR2dZ=0.0;
      CCOR2dR=1.0;
    elseif H1 == 0 % E1 -> -Inf and E2 !-> +/-Inf
      EX2=exp((ALT-ZH)/H2);
      EX2dZ=EX2/H2;
      CCOR2=R/(1.+.5*EX2);
      CCOR2dZ=-CCOR2*(.5*EX2dZ)/(1.+.5*EX2);
      CCOR2dR=1.0/(1.+.5*EX2);
    else % E2 -> -Inf and E1 !-> +/-Inf
      EX1=exp((ALT-ZH)/H1);
      EX1dZ=EX1/H1;
      CCOR2=R/(1.+.5*EX1);
      CCOR2dZ=-CCOR2*(.5*EX1dZ)/(1.+.5*EX1);
      CCOR2dR=1.0/(1.+.5*EX1);
    end
  else
    E1=(ALT-ZH)/H1;
    E2=(ALT-ZH)/H2;
    if E1 > CIRA.ARGMAX || E2 > CIRA.ARGMAX % E1 -> Inf or E2 -> Inf
      CCOR2=0.; % CCOR2 < R*2.0*3.975e-31
      CCOR2dZ=0.0;
      CCOR2dR=0.0;
    elseif E1 < -CIRA.ARGMAX && E2 < -CIRA.ARGMAX % E1 -> -Inf and E2 -> -Inf
      CCOR2=R; % CCOR2 > R - R*3.975e-31
      CCOR2dZ=0.0;
      CCOR2dR=1.0;
    elseif E1 < -CIRA.ARGMAX % E1 -> -Inf and E2 !-> +/-Inf
      EX2=exp(E2);
      EX2dZ=EX2/H2;
      CCOR2=R/(1.+.5*EX2);
      CCOR2dZ=-CCOR2*(.5*EX2dZ)/(1.+.5*EX2);
      CCOR2dR=1.0/(1.+.5*EX2);
    elseif E2 < -CIRA.ARGMAX % E2 -> -Inf and E1 !-> +/-Inf
      EX1=exp(E1);
      EX1dZ=EX1/H1;
      CCOR2=R/(1.+.5*EX1);
      CCOR2dZ=-CCOR2*(.5*EX1dZ)/(1.+.5*EX1);
      CCOR2dR=1.0/(1.+.5*EX1);
    else % E1 !-> +/- Inf and E2 !-> +/- Inf
      EX1=exp(E1);
      EX2=exp(E2);
      EX1dZ=EX1/H1;
      EX2dZ=EX2/H2;
      CCOR2=R/(1.+.5*(EX1+EX2));
      CCOR2dZ=-CCOR2*(.5*(EX1dZ+EX2dZ))/(1.+.5*(EX1+EX2));
      CCOR2dR=1.0/(1.+.5*(EX1+EX2));
    end
  end
  CCOR2=exp(CCOR2);
  CCOR2dZ=CCOR2*CCOR2dZ;
  CCOR2dR=CCOR2*CCOR2dR;
end

