function [ sg0,SG0dEX ] = SG0( EX, P25, P26, AP, defAp )
  %       3hr Magnetic activity functions
  % SG0   Eq. A24a
  if EX ~= 1
    f = (1.-EX^8)/(1.-EX);
    fdEX = (f-8*EX^7)/(1.-EX);
  else
    f = 8;
    fdEX = 8*7;
  end
  if P25 < 1.E-4
    P25 = 1.E-4;
  end
  P25 = abs(P25);
  [SEX,SEXdEX] = SUMEX(EX);
  a = [G0(AP(CIRA.CURRENT_AP        )-defAp,P25,P26),...
       G0(AP(CIRA.CURRENT_M_3_AP    )-defAp,P25,P26),...
       G0(AP(CIRA.CURRENT_M_6_AP    )-defAp,P25,P26),...
       G0(AP(CIRA.CURRENT_M_9_AP    )-defAp,P25,P26),...
       G0(AP(CIRA.CURRENT_M_12_33_AP)-defAp,P25,P26),...
       G0(AP(CIRA.CURRENT_M_36_57_AP)-defAp,P25,P26)];
  num=  a(1) + (a(2) + (a(3) + (a(4) + (a(5) + a(6)*EX^8)*f*EX)*EX)*EX)*EX;
  numdEX=      (a(2) + (a(3)*2 + (a(4)*3 + (a(5)*4 + a(6)*12*EX^8)*f*EX)*EX)*EX) ...
                                         + (a(5) + a(6)*EX^8)*EX^4*fdEX;
  sg0=num/SEX;
  SG0dEX=numdEX/SEX - sg0*SEXdEX/SEX;
end
function [ g0 ] = G0( dA, P25, P26 )
  % G0 Eq. A24d
  g0=(dA+(P26-1.)*(dA+(exp(-P25*dA)-1.)/P25));
end
function [ sumEX, sumEXdEX ] = SUMEX( EX )
  % SUMEX   Eq. A24c
  if EX == 0
    sumEX = 1.0;
    sumEXdEX=Inf;
  elseif EX == 1
    sumEX = 20.0;
    sumEXdEX = 19*18.5;
  elseif EX < 0
    SQEX = abs(EX)^(.5);
    sumEX = complex(1.0,(1.-EX^19)/(1.-EX)*SQEX);
    sumEXdEX=complex(Inf,-(1.-EX^19)/(1.-EX)*0.5/SQEX);
  else
    SQEX = EX^(.5);
    sumEX=(1.-EX^19)/(1.-EX)*SQEX;
    sumEXdEX=-19*EX^18/(1.-EX)*SQEX+sumEX/(1.-EX)+sumEX*0.5/EX;
    sumEX = sumEX + 1.0;
  end
end
