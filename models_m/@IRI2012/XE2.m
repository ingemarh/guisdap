function [ XE2,context,dXE2 ] = XE2( context, H )
%XE2 ELECTRON DENSITY FOR THE BOTTOMSIDE F-REGION
%        REAL FUNCTION XE2(H)                         
% ELECTRON DENSITY FOR THE BOTTOMSIDE F-REGION (HMF1...HMF2).                   

%        COMMON    /BLOCK1/HMF2,XNMF2,HMF1,F1REG
%     &          /BLOCK2/B0,B1,C1        /ARGEXP/ARGMAX
%	    logical	f1reg

  x=(context.HMF2-H)/context.B0;
  dx=-1.0/context.B0;
  if x <= 0.0
    x=0.0;
  end
  z=x^context.B1;
  dz=context.B1*x^(context.B1-1.0)*dx;
  if z > IRI2012.ARGMAX
    z=IRI2012.ARGMAX;
  end
  XE2=context.NMF2*exp(-z)/cosh(x);
  dXE2=-dz*XE2 - XE2*sinh(x)*dx/cosh(x);

end

