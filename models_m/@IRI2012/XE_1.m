function [ XE_1, dXE_1 ] = XE_1( context, H )
%XE_1 ELECTRON DENSITY BETWEEN HA(KM) AND 1000 KM
%        REAL FUNCTION XE_1(H)                          
% ELECTRON DENSITY BEETWEEN HA(KM) AND 1000 KM     
% SUMMARIZING PROCEDURES  NE1....6;                

%        COMMON    /BLOCK1/HMF2,XNMF2,XHMF1,F1REG         
%     &          /BLOCK3/HZ,T,HST
%     &        /BLOCK4/HME,XNME,HEF
%	    logical 	f1reg
  if context.F1REG
    HMF1=context.HMF1;
  else
    HMF1=context.HMF2;
  end
  if H >= context.HMF2
    [XE_1,dXE_1]=context.XE1(H);
  elseif H >= HMF1
    [XE_1,~,dXE_1]=context.XE2(H);
  elseif H >= context.HZ
    [XE_1,~,dXE_1]=context.XE3_1(H);
  elseif H >= context.HEF
    [XE_1,dXE_1]=context.XE4_1(H);
  elseif H >= context.HME
    [XE_1,dXE_1]=context.XE5(H);
  else
    [XE_1,dXE_1]=context.XE6(H);
  end
end

