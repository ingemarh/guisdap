function [ C ] = SPHARM( L,M,COLAT,AZ )
%SPHARM CALCULATES THE COEFFICIENTS OF THE SPHERICAL HARMONIC
%
%      SUBROUTINE SPHARM(C,L,M,COLAT,AZ)            
% CALCULATES THE COEFFICIENTS OF THE SPHERICAL HARMONIC                         
% EXPANSION THAT WAS USED FOR THE BRACE-THEIS-MODELS.                           

%      DIMENSION C(82)                              
  C = zeros(82,1);
  C(1)=1.;
  K=2;
  X=cos(COLAT);
  C(K)=X;
  K=K+1;
  for I=2:L
    C(K)=((2*I-1)*X*C(K-1)-(I-1)*C(K-2))/I;
    K=K+1;
  end
  Y=sin(COLAT);
  for MT=1:M
    CAZ=cos(MT*AZ);
    SAZ=sin(MT*AZ);
    C(K)=Y^MT;
    K=K+1;
    if MT ~= L
      C(K)=C(K-1)*X*(2*MT+1);
      K=K+1;
      if (MT+1) ~= L
        for I=2+MT:L
          C(K)=((2*I-1)*X*C(K-1)-(I+MT-1)*C(K-2))/(I-MT);
          K=K+1;
        end
      end
    end
    N=L-MT+1;
    for I=1:N
      C(K)=C(K-N)*CAZ;
      C(K-N)=C(K-N)*SAZ;
      K=K+1;
    end
  end

end

