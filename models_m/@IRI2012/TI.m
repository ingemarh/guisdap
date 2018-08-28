function [ TI,dTI ] = TI( context, H )
%TI ION TEMPERATURE FOR HEIGHTS NOT GREATER 1000 KM
%                     
%*************************************************************                  
%**************** ION TEMPERATURE ****************************
%*************************************************************                  
%
%
%      REAL FUNCTION TI(H)
%----------------------------------------------------------------
% ION TEMPERATURE FOR HEIGHTS NOT GREATER 1000 KM AND NOT LESS HS               
% EXPLANATION SEE FUNCTION RPID.                   
%----------------------------------------------------------------

%      REAL              MM
%      COMMON  /BLOCK8/  HS,TNHS,XSM(4),MM(5),DTI(4),MXSM

  SUM=context.MM(1)*(H-context.HS)+context.TNHS;
  dSUM=context.MM(1);
  for I=1:context.MXSM-1
    [AA,dAA] = IRI2012.EPTR(H ,context.DTI(I),context.XSM(I));
    BB = IRI2012.EPTR(context.HS,context.DTI(I),context.XSM(I));
    SUM=SUM+(context.MM(I+1)-context.MM(I))*(AA-BB)*context.DTI(I);
    dSUM=dSUM+(context.MM(I+1)-context.MM(I))*dAA*context.DTI(I);
  end
  TI=SUM;
  dTI=dSUM;

end

