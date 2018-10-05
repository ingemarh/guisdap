function [ DXE1N ] = DXE1N( context, H )
%DXE1N LOGARITHMIC DERIVATIVE OF FUNCTION XE1
%        FUNCTION DXE1N(H)                            
% LOGARITHMIC DERIVATIVE OF FUNCTION XE1 (KM-1).   

%        COMMON    /BLOCK1/HMF2,XNMF2,HMF1,F1REG
%     &            /BLO10/BETA,ETA,DELTA,ZETA                    
%	    logical f1reg

  x0 = 300. - context.DELTA;
  x=(H-context.HMF2)/(1000.0-context.HMF2)*700.0 + x0;
  epst2 = IRI2012.EPST(x,100.0,300.0);
  epst1 = IRI2012.EPST(x,context.BETA ,394.5);
  DXE1N = - context.ETA * epst1 + context.ZETA * (1. - epst2)  ;           

end

