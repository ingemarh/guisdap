function [ XE3_1,context,dXE3_1 ] = XE3_1( context, H )
%XE3_1 ELECTRON DENSITY FOR THE F1-LAYER
%        REAL FUNCTION XE3_1(H)
% ELECTRON DENSITY FOR THE F1-LAYER (HZ.....HMF1)
% USING THE NEW DEFINED F1-LAYER FUNCTION (Reinisch and Huang, Advances 
% in Space Research, Volume 25, Number 1, 81-88, 2000)

%        COMMON	/BLOCK1/	HMF2,XNMF2,HMF1,F1REG
%     &		/BLOCK2/	B0,B1,D1F1
%	    logical	f1reg
%
  if context.F1REG
    H1BAR=context.HMF1*(1.0-((context.HMF1-H)/ ...
      context.HMF1)^(1.0+context.C1));
    dH1BAR=(1.0+context.C1)*((context.HMF1-H)/context.HMF1)^(context.C1);
  else
    H1BAR = H;
    dH1BAR = 1.0;
  end
  [XE3_1,context,dXE3_1]=context.XE2(H1BAR);
  dXE3_1 = dXE3_1*dH1BAR;

end

