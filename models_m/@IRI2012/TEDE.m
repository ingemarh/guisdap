function [ TEDE ] = TEDE( H,DEN,COV )
%TEDE ELECTRON TEMEPERATURE MODEL
%
%      FUNCTION TEDE(H,DEN,COV)                     
% ELECTRON TEMEPERATURE MODEL AFTER BRACE,THEIS .  
% FOR NEG. COV THE MEAN COV-INDEX (3 SOLAR ROT.) IS EXPECTED.                   
% DEN IS THE ELECTRON DENSITY IN M-3.              
  Y=1051.+(17.01*H-2746.)* ...
   exp(-5.122E-4*H+(6.094E-12-3.353E-14*H)*DEN);
  ACOV=abs(COV);
  if COV < 0.   
    YC=1.+(.123+1.69E-3*ACOV)/(1.+exp(-(ACOV-115.)/10.));
  else
    YC=1.+(.117+2.02E-3*ACOV)/(1.+exp(-(ACOV-102.5)/5.));
  end
  TEDE=Y*YC;

end

