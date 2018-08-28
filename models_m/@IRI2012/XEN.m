function [ XEN,dXEN ] = XEN( context,H,HMF2,XNMF2,HME,NL,HX,SC,AMP )
%XEN ELECTRON DENSITY WITH NEW MIDDLE IONOSPHERE
%
%        REAL FUNCTION XEN(H,HMF2,XNMF2,HME,NL,HX,SC,AMP)
%----------------------------------------------------------------------
% ELECTRON DENSITY WITH NEW MIDDLE IONOSPHERE
%----------------------------------------------------------------------

%        DIMENSION       HX(NL),SC(NL),AMP(NL)
%
  if H >= HMF2
    [XEN,dXEN] = context.XE1(H);
  elseif H >= HME
    [XEN,dXEN] = IRI2012.XE2TO5(H,HMF2,NL,HX,SC,AMP);
    XEN = XNMF2 * XEN;
    dXEN = XNMF2 * dXEN;
  else
    [XEN,dXEN] = context.XE6(H);
  end


end

