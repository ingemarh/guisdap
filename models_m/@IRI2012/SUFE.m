function [ FE ] = SUFE( FIELD,RFE,M )
%SUFE SELECTS THE REQUIRED ION DENSITY PARAMETER SET
%
%      SUBROUTINE SUFE (FIELD,RFE,M,FE)             
% SELECTS THE REQUIRED ION DENSITY PARAMETER SET.
% THE INPUT FIELD INCLUDES DIFFERENT SETS OF DIMENSION M EACH                
% CARACTERISED BY 4 HEADER NUMBERS. RFE(4) SHOULD CONTAIN THE                   
% CHOSEN HEADER NUMBERS.FE(M) IS THE CORRESPONDING SET.                         

%      DIMENSION RFE(4),FE(12),FIELD(80),EFE(4)
  NE = 4;
  FE = zeros(12,1);
  EFE = zeros(NE,1);
  K=0;
  while true
    for I=1:NE   
      K=K+1;
      EFE(I)=FIELD(K);
    end
    for I=1:M    
      K=K+1;
      FE(I)=FIELD(K);
    end
    found = false;
    for I=1:NE
     if ((EFE(I) > -10.0) && (RFE(I) ~= EFE(I)))
       found = true;
       break;
     end
    end
    if ~found
      break;
    end
  end
end

