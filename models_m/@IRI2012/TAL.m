function [ AUS6,SPT ] = TAL( SHABR,SDELTA,SHBR,SDTDH0 )
%TAL CALCULATES THE COEFFICIENTS SPT FOR THE POLYNOMIAL
%      SUBROUTINE TAL(SHABR,SDELTA,SHBR,SDTDH0,AUS6,SPT)                         
% CALCULATES THE COEFFICIENTS SPT FOR THE POLYNOMIAL
% Y(X)=1+SPT(1)*X**2+SPT(2)*X**3+SPT(3)*X**4+SPT(4)*X**5               
% TO FIT THE VALLEY IN Y, REPRESENTED BY:                
% Y(X=0)=1, THE X VALUE OF THE DEEPEST VALLEY POINT (SHABR),                    
% THE PRECENTAGE DEPTH (SDELTA), THE WIDTH (SHBR) AND THE                       
% DERIVATIVE DY/DX AT THE UPPER VALLEY BOUNDRY (SDTDH0).                        
% IF THERE IS AN UNWANTED ADDITIONAL EXTREMUM IN THE VALLEY                     
% REGION, THEN AUS6=.TRUE., ELSE AUS6=.FALSE..     
% FOR -SDELTA THE COEFF. ARE CALCULATED FOR THE FUNCTION                        
% Y(X)=EXP(SPT(1)*X**2+...+SPT(4)*X**5).           

%      DIMENSION SPT(4)                             
%      LOGICAL AUS6    
  SPT = zeros(1,4);
  Z1=-SDELTA/(100.0*SHABR*SHABR);
  if (SDELTA <= 0.)
    Z1=log(1.+SDELTA/100.)/(SHABR*SHABR);
  end
  Z3=SDTDH0/(2.*SHBR);
  Z4=SHABR-SHBR;
  SPT(4)=2.0*(Z1*(SHBR-2.0*SHABR)*SHBR+Z3*Z4*SHABR)/ ...
    (SHABR*SHBR*Z4*Z4*Z4);
  SPT(3)=Z1*(2.0*SHBR-3.0*SHABR)/(SHABR*Z4*Z4)- ...
    (2.*SHABR+SHBR)*SPT(4);
  SPT(2)=-2.0*Z1/SHABR-2.0*SHABR*SPT(3)-3.0*SHABR*SHABR*SPT(4);
  SPT(1)=Z1-SHABR*(SPT(2)+SHABR*(SPT(3)+SHABR*SPT(4)));
  AUS6=false;
  B=4.*SPT(3)/(5.*SPT(4))+SHABR;
  C=-2.*SPT(1)/(5*SPT(4)*SHABR);
  Z2=B*B/4.-C;
  if (Z2 >= 0.0)
    Z3=sqrt(Z2);
    Z1=B/2.;
    Z2=-Z1+Z3;
    if Z2 > 0.0 && Z2 < SHBR
      AUS6=true;
    end
    if (abs(Z3) <= 1.E-15)
      Z2=C/Z2;
      if Z2 > 0.0 && Z2 < SHBR
        AUS6=true;
      end
    else
      Z2=-Z1-Z3;
      if Z2 > 0.0 && Z2 < SHBR
        AUS6=true;
      end
    end
  end

end

