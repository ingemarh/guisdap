function [ FL,ICODE,B0 ] = SHELLG( context, GLAT,GLON,ALT,DIMO )
%SHELLG Summary of this function goes here
%   Detailed explanation goes here
% SHELLIG.FOR
%
% 11/01/91 SHELLG: lowest starting point for B0 search is 2  
%  1/27/92 Adopted to IGRF-91 coeffcients model
%  2/05/92 Reduce variable-names: INTER(P)SHC,EXTRA(P)SHC,INITI(ALI)ZE
%  8/08/95 Updated to IGRF-45-95; new coeff. DGRF90, IGRF95, IGRF95S
%  5/31/00 Updated to IGRF-45-00; new coeff.: IGRF00, IGRF00s
%  3/24/05 Updated to IGRF-45-10; new coeff.: IGRF05, IGRF05s
%  4/25/05 ENTRY FELDI(XI,H) and  DO 1111 I=1,7 [Alexey Petrov]
%  7/22/09 SHELLG: NMAX=13 for DGRF00 and IGRF05; H/G-arrays(195)
%  2/26/10 FELDCOF: Updated IGRF45-15; new coeff: DGRF05, IGRF10, IGRF10S
%  4/29/10 H/H-arrays(196); FELDCOF: corrected IGRF00 and ..00S
%  4/29/10 Change to new files dgrf%%%%.asc; new GETSHC; char*12 to 13
%
%*********************************************************************
%  SUBROUTINES SHELLG, STOER, FELDG, FELDCOF, GETSHC,        *
%       INTERSHC, EXTRASHC, INITIZE                                  *
%*********************************************************************
%*********************************************************************
%
%
%
%      SUBROUTINE SHELLG(GLAT,GLON,ALT,DIMO,FL,ICODE,B0)
%-----------------------------------------------------------------------        
% CALCULATES L-VALUE FOR SPECIFIED GEODAETIC COORDINATES, ALTITUDE
% AND GEMAGNETIC FIELD MODEL.
% REF: G. KLUGE, EUROPEAN SPACE OPERATIONS CENTER, INTERNAL NOTE 
%      NO. 67, 1970.
%      G. KLUGE, COMPUTER PHYSICS COMMUNICATIONS 3, 31-35, 1972
%-----------------------------------------------------------------------        
% CHANGES (D. BILITZA, NOV 87):
%   - USING CORRECT DIPOL MOMENT I.E.,DIFFERENT COMMON/MODEL/
%   - USING IGRF EARTH MAGNETIC FIELD MODELS FROM 1945 TO 1990
% 09/07/22 NMAX=13 for DGRF00 and IGRF05; H/G-arrays(195)
%-----------------------------------------------------------------------        
%  INPUT:  ENTRY POINT SHELLG
%             GLAT  GEODETIC LATITUDE IN DEGREES (NORTH)
%             GLON  GEODETIC LONGITUDE IN DEGREES (EAST)
%             ALT   ALTITUDE IN KM ABOVE SEA LEVEL
%
%          ENTRY POINT SHELLC
%             V(3)  CARTESIAN COORDINATES IN EARTH RADII (6371.2 KM)
%                     X-AXIS POINTING TO EQUATOR AT 0 LONGITUDE
%                     Y-AXIS POINTING TO EQUATOR AT 90 LONG.
%                     Z-AXIS POINTING TO NORTH POLE
%
%          DIMO     DIPOL MOMENT IN GAUSS (NORMALIZED TO EARTH RADIUS) 
%
%          COMMON 
%             X(3)    NOT USED
%             H(144)  FIELD MODEL COEFFICIENTS ADJUSTED FOR SHELLG
%-----------------------------------------------------------------------        
%  OUTPUT: FL           L-VALUE
%          ICODE        =1 NORMAL COMPLETION
%                       =2 UNPHYSICAL CONJUGATE POINT (FL MEANINGLESS)
%                       =3 SHELL PARAMETER GREATER THAN LIMIT UP TO
%                          WHICH ACCURATE CALCULATION IS REQUIRED;
%                          APPROXIMATION IS USED.
%          B0           MAGNETIC FIELD STRENGTH IN GAUSS
%-----------------------------------------------------------------------        
%      DIMENSION         V(3),U(3,3),P(8,100),SP(3)
%      COMMON/IGRF2/     X(3),H(196)
%      COMMON/FIDB0/     SP
%      COMMON/IGRF1/     UMR,ERA,AQUAD,BQUAD
%
%-- RMIN, RMAX ARE BOUNDARIES FOR IDENTIFICATION OF ICODE=2 AND 3
%-- STEP IS STEP SIZE FOR FIELD LINE TRACING
%-- STEQ IS STEP SIZE FOR INTEGRATION
% 
  persistent RMIN RMAX MAXN;
  if isempty(RMIN)
    RMIN = 0.05;
    RMAX = 1.01;
    MAXN = 100;
  end
  STEP = 0.20;
  STEQ = 0.03;
  %BEQU = 1.E10;
  %IEQU = 0;
  P = zeros(8,MAXN);
  %*****ENTRY POINT  SHELLG  TO BE USED WITH GEODETIC CO-ORDINATES
  XI = IGRF.newVector();
  [ XI(1),XI(2),XI(3) ] = IGRF.GEODETIC2CARTESIAN( GLAT, GLON, ALT );
  XI = XI/context.ERA;
  %      GOTO 9                                                     
  %      ENTRY SHELLC(V,FL,B0)                                     
  %*****ENTRY POINT  SHELLC  TO BE USED WITH CARTESIAN CO-ORDINATES
  %      X(1)=V(1);                                                  
  %      X(2)=V(2);                                                  
  %      X(3)=V(3);                                                  
  %*****CONVERT TO DIPOL-ORIENTED CO-ORDINATES                     
  %9
  RQ=1./(XI(1)^2+XI(2)^2+XI(3)^2);
  R3H=sqrt(RQ*sqrt(RQ));                                      
  P(1,2)=(XI(1)*IGRF.U(1,1)+XI(2)*IGRF.U(2,1)+XI(3)*IGRF.U(3,1))*R3H;           
  P(2,2)=(XI(1)*IGRF.U(1,2)+XI(2)*IGRF.U(2,2)                  )*R3H;           
  P(3,2)=(XI(1)*IGRF.U(1,3)+XI(2)*IGRF.U(2,3)+XI(3)*IGRF.U(3,3))*RQ;            
  %*****FIRST THREE POINTS OF FIELD LINE                           
  if P(3,2) >= 0
    STEP=-abs(STEP);
  else
    STEP=abs(STEP);
  end
  [P,BQ2,R2] = context.STOER(P,2);                                  
  B0=sqrt(BQ2);                                               
  P(1,3)=P(1,2)+0.5*STEP*P(4,2);                              
  P(2,3)=P(2,2)+0.5*STEP*P(5,2);                              
  P(3,3)=P(3,2)+0.5*STEP;                                     
  [P,~,~] = context.STOER(P,3);                                  
  P(1,1)=P(1,2)-STEP*(2.*P(4,2)-P(4,3));                      
  P(2,1)=P(2,2)-STEP*(2.*P(5,2)-P(5,3));                      
  P(3,1)=P(3,2)-STEP;                                         
  [P,BQ1,~] = context.STOER(P,1);                                  
  P(1,3)=P(1,2)+STEP*(20.*P(4,3)-3.*P(4,2)+P(4,1))/18.;       
  P(2,3)=P(2,2)+STEP*(20.*P(5,3)-3.*P(5,2)+P(5,1))/18.;       
  P(3,3)=P(3,2)+STEP;                                         
  [P,BQ3,~] = context.STOER(P,3);                                  
  %*****INVERT SENSE IF REQUIRED                                   
  if BQ3 > BQ1
    STEP=-STEP;                                                 
    %R3=R1;                                                      
    %BQ3=BQ1;
    for I=1:7                                                 
      ZZ=P(I,1);                                                  
      P(I,1)=P(I,3);                                              
      P(I,3)=ZZ;
    end
  end
  %*****SEARCH FOR LOWEST MAGNETIC FIELD STRENGTH
  %if BQ1 < BEQU
    %BEQU=BQ1;
    %IEQU=1;
  %end
  %if BQ2 < BEQU
    %BEQU=BQ2;
    %IEQU=2;
  %end
  %if BQ3 < BEQU
    %BEQU=BQ3;
    %IEQU=3;
  %end
  %*****INITIALIZATION OF INTEGRATION LOOPS                        
  STEP12=STEP/12.;
  STEP2=STEP+STEP;
  if STEP >= 0
    STEQ=abs(STEQ);                                       
  else
    STEQ=-abs(STEQ);                                       
  end
  FI=0.;                                                      
  ICODE=1;                                                    
  ORADIK=0.;                                                  
  OTERM=0.;                                                   
  STP=R2*STEQ;                                                
  Z=P(3,2)+STP;                                               
  STP=STP/0.75;
  P(8,1)=STEP2*(P(1,1)*P(4,1)+P(2,1)*P(5,1));                 
  P(8,2)=STEP2*(P(1,2)*P(4,2)+P(2,2)*P(5,2));
  %*****MAIN LOOP (FIELD LINE TRACING)                             
  for N=3:MAXN
    %*****CORRECTOR (FIELD LINE TRACING)                             
    P(1,N)=P(1,N-1)+STEP12*(5.*P(4,N)+8.*P(4,N-1)-P(4,N-2));    
    P(2,N)=P(2,N-1)+STEP12*(5.*P(5,N)+8.*P(5,N-1)-P(5,N-2));    
    %*****PREPARE EXPANSION COEFFICIENTS FOR INTERPOLATION           
    %*****OF SLOWLY VARYING QUANTITIES                               
    P(8,N)=STEP2*(P(1,N)*P(4,N)+P(2,N)*P(5,N));                 
    C0=P(1,N-1)^2+P(2,N-1)^2;                                 
    C1=P(8,N-1);                                                
    C2=(P(8,N)-P(8,N-2))*0.25;                                  
    C3=(P(8,N)+P(8,N-2)-C1-C1)/6.0;
    D0=P(6,N-1);                                                
    D1=(P(6,N)-P(6,N-2))*0.5;
    D2=(P(6,N)+P(6,N-2)-D0-D0)*0.5;                             
    E0=P(7,N-1);
    E1=(P(7,N)-P(7,N-2))*0.5;                                   
    E2=(P(7,N)+P(7,N-2)-E0-E0)*0.5;                             
    %*****INNER LOOP (FOR QUADRATURE)
    found = false;
    while ~found
      T=(Z-P(3,N-1))/STEP;
      if T > 1.
        break;
      end
      HLI=0.5*(((C3*T+C2)*T+C1)*T+C0);
      ZQ=Z*Z;
      R=HLI+sqrt(HLI*HLI+ZQ);
      if R <= RMIN
        %*****APPROXIMATION FOR HIGH VALUES OF L.                               
        ICODE=3;                                                           
        T=-P(3,N-1)/STEP;                                                  
        FL=1./(abs(((C3*T+C2)*T+C1)*T+C0)+1E-15);
        return;
      end
      RQ=R*R;
      FF=sqrt(1.+3.*ZQ/RQ);
      RADIK=B0-((D2*T+D1)*T+D0)*R*RQ*FF;
      if R-RMAX > 0
        ICODE=2;
        RADIK=RADIK-12.*(R-RMAX)^2;
      end
      if RADIK+RADIK <= ORADIK
        found = true;
      else
        TERM=sqrt(RADIK)*FF*((E2*T+E1)*T+E0)/(RQ+ZQ);
        FI=FI+STP*(OTERM+TERM);
        ORADIK=RADIK;
        OTERM=TERM;
        STP=R*STEQ;
        Z=Z+STP;
      end
    end
    if found
      break;
    end
    %*****PREDICTOR (FIELD LINE TRACING)                    
    P(1,N+1)=P(1,N)+STEP12*(23.*P(4,N)-16.*P(4,N-1)+5.*P(4,N-2));  
    P(2,N+1)=P(2,N)+STEP12*(23.*P(5,N)-16.*P(5,N-1)+5.*P(5,N-2));  
    P(3,N+1)=P(3,N)+STEP;                                          
    [P,~,~] = context.STOER(P,N+1);                                   
    %*****SEARCH FOR LOWEST MAGNETIC FIELD STRENGTH
    %if BQ3 < BEQU
      %IEQU=N+1;
      %BEQU=BQ3;
    %end
  end
  %if IEQU < 2
  %  IEQU=2;
  %end
  %context.FIDB0.SP(1)=P(1,IEQU-1);
  %context.FIDB0.SP(2)=P(2,IEQU-1);
  %context.FIDB0.SP(3)=P(3,IEQU-1);
  if ORADIK >= 1E-15
    FI=FI+STP/0.75*OTERM*ORADIK/(ORADIK-RADIK);
  end
  %
  %-- The minimal allowable value of FI was changed from 1E-15 to 1E-12,
  %-- because 1E-38 is the minimal allowable arg. for ALOG in our envir.
  %-- D. Bilitza, Nov 87.
  %
  FI=0.5*abs(FI)/sqrt(B0)+1E-12;                       
  %
  %*****COMPUTE L FROM B AND I.  SAME AS CARMEL IN INVAR.  
  %
  %-- Correct dipole moment is used here. D. Bilitza, Nov 87.
  %
  DIMOB0=DIMO/B0;
  arg1=log(FI);
  arg2=log(DIMOB0);
  %      arg = FI*FI*FI/DIMOB0
  %      if(abs(arg) > 88.0) arg=88.0;
  XX=3*arg1-arg2;
  if XX > 23.0
    cc=[1,-3.0460681E0];                                                 
  elseif XX > 11.7
    cc=[2.8212095E-8,-3.8049276E-6,2.170224E-4,- ...
      6.7310339E-3,1.2038224E-1,-1.8461796E-1, ...
      2.0007187E0];             
  elseif XX > +3.0
    cc=[6.3271665E-10,-3.958306E-8,9.9766148E-07,- ...
      1.2531932E-5,7.9451313E-5,-3.2077032E-4, ...
      2.1680398E-3,1.2817956E-2,4.3510529E-1, ...
      6.222355E-1];                  
  elseif XX > -3.0
    cc=[2.6047023E-10,2.3028767E-9,-2.1997983E-8,- ...
      5.3977642E-7,-3.3408822E-6,3.8379917E-5, ...
      1.1784234E-3,1.4492441E-2,4.3352788E-1, ...
      6.228644E-1];
  elseif XX > -22.
    cc=[-8.1537735E-14,8.3232531E-13,1.0066362E-9, ...
      8.1048663E-8,3.2916354E-6,8.2711096E-5, ...
      1.3714667E-3,1.5017245E-2,4.3432642E-1, ...
      6.2337691E-1];
  else
    cc = [3.33338E-1,3.0062102E-1];
  end
  GG = polyval(cc,XX);
  FL=exp(log((1.+exp(GG))*DIMOB0)/3.0);

end

