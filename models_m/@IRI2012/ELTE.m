function [ ELTE,dELTE ] = ELTE( context, H )
%ELTE ELECTRON TEMPERATURE PROFILE
%
%      REAL FUNCTION ELTE(H)
%----------------------------------------------------------------
% ELECTRON TEMPERATURE PROFILE BASED ON THE TEMPERATURES AT 7 FIXED
% HEIGHTS (AH(7)) AND THE TEMPERATURE GRADIENTS BETWEEN THESE THESE 
% HEIGHTS (ST(6)) GIVEN IN THE COMMON BLOCK. ATE1 IS THE TEMPERATURE
% AT THE STARTING HEIGHT 120 KM. D(5) DEFINE THE TRANSITION SPAN FROM
% ONE CONSTANT GRADIENT REGION TO THE NEXT.
%----------------------------------------------------------------

%      COMMON /BLOTE/AH(7),ATE1,ST(6),D(5)
%
  H = cast(H,IRI2012.float_t);
  SUM=context.ATE1+context.STTE(1)*(H-context.AHH(1));                     
  dSUM=context.STTE(1);                     
  for I=1:5
    if IRI2012.DTE(I) == 0.0
      if H > context.AHH(I+1)
        AA = ( H - context.AHH(I+1) );
        dAA = 1.0;
      else
        AA = 0.0;
        dAA = 0.0;
      end
      if context.AHH(1) > context.AHH(I+1)
        BB = ( context.AHH(1) - context.AHH(I+1) );
      else
        BB = 0.0;
      end
      SUM=SUM+context.STTE(I+1)+(context.STTE(I+1)-context.STTE(I))*(AA-BB);
      dSUM=dSUM+(context.STTE(I+1)-context.STTE(I))*dAA;
    else
      [AA,dAA] = IRI2012.EPTR(H    ,IRI2012.DTE(I),context.AHH(I+1));
      BB = IRI2012.EPTR(context.AHH(1),IRI2012.DTE(I),context.AHH(I+1));
      SUM=SUM+(context.STTE(I+1)-context.STTE(I))*(AA-BB)*IRI2012.DTE(I);
      dSUM=dSUM+(context.STTE(I+1)-context.STTE(I))*dAA*IRI2012.DTE(I);
    end
  end
  ELTE=SUM;
  dELTE=dSUM;

end

