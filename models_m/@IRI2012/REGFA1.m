function [ SCHALT,X,context ] = REGFA1( context,X11,X22,FX11,FX22,EPS,FW,F )
%REGFA REGULA-FALSI-PROCEDURE
%
%************************************************************                   
%*********** INTERPOLATION AND REST ***************************                 
%**************************************************************                 
%
%
%      SUBROUTINE REGFA1(X11,X22,FX11,FX22,EPS,FW,F,SCHALT,X) 
% REGULA-FALSI-PROCEDURE TO FIND X WITH F(X)-FW=0. X1,X2 ARE THE                
% STARTING VALUES. THE COMUTATION ENDS WHEN THE X-INTERVAL                      
% HAS BECOME LESS THAN EPS . IF SIGN(F(X1)-FW)= SIGN(F(X2)-FW)                  
% THEN SCHALT=.TRUE.  

%      LOGICAL L1,LINKS,K,SCHALT                    
  SCHALT=false;
  EP=EPS;
  X1=X11;
  X2=X22;
  F1=FX11-FW;
  F2=FX22-FW;
  K=false;
  NG=2;
  LFD=0;
  if F1*F2 > 0.0 % on same side of zero
    X=0.0;
    SCHALT=true;
    return;
  end
  X=(X1*F2-X2*F1)/(F2-F1);
  while abs(X2-X1) > EP
    [FX,context]=F(context,X);
    FX=FX-FW;
    LFD=LFD+1;
    if LFD > 20
      EP=EP*10.;
      LFD=0;
    end
    LINKS=(F1*FX > 0.0);
    K=~K;
    if LINKS
      X1=X;
      F1=FX;
    else
      X2=X;
      F2=FX;
    end
    if abs(X2-X1) > EP
      if K
        L1=LINKS;
        DX=(X2-X1)/NG;
        if ~LINKS
          DX=DX*(NG-1);
        end
        X=X1+DX;
      else
        if (LINKS && (~L1)) || (~LINKS && L1)
          NG=2*NG;
        end
        X=(X1*F2-X2*F1)/(F2-F1);
      end
    end
  end

end

