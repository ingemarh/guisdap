function [ P,BQ,R ] = STOER( context, P, k )
%STOER SUBROUTINE USED FOR FIELD LINE TRACING IN SHELLG
%      SUBROUTINE STOER(P,BQ,R)                                          
%*******************************************************************
%* SUBROUTINE USED FOR FIELD LINE TRACING IN SHELLG                *
%* CALLS ENTRY POINT FELDI IN GEOMAGNETIC FIELD SUBROUTINE FELDG   *
%                                                                  *
% 09/07/22 NMAX=13 for DGRF00 and IGRF05; H/G-arrays(195)          *
%*******************************************************************

%      DIMENSION         P(7),U(3,3)
%      COMMON/IGRF2/     XI(3),H(196)
  %*****XM,YM,ZM  ARE GEOMAGNETIC CARTESIAN INVERSE CO-ORDINATES          
  ZM=P(3,k);                                                           
  FLI=P(1,k)*P(1,k)+P(2,k)*P(2,k)+1E-15;
  R=0.5*(FLI+sqrt(FLI*FLI+(ZM+ZM)^2));
  RQ=R*R;
  WR=sqrt(R);                                                        
  XM=P(1,k)*WR;                                                        
  YM=P(2,k)*WR;                                                        
  %*****TRANSFORM TO GEOGRAPHIC CO-ORDINATE SYSTEM                        
  XI(1)=XM*IGRF.U(1,1)+YM*IGRF.U(1,2)+ZM*IGRF.U(1,3);                               
  XI(2)=XM*IGRF.U(2,1)+YM*IGRF.U(2,2)+ZM*IGRF.U(2,3);                               
  XI(3)=XM*IGRF.U(3,1)          +ZM*IGRF.U(3,3);                               
  %*****COMPUTE DERIVATIVES                                               
  %      CALL FELDI(XI,H)                                                  
  H = context.FELDI( 1.0, XI(1), XI(2), XI(3), 3 );
  Q=H(1)/RQ;                                                         
  DX=H(3)+H(3)+Q*XI(1);                                              
  DY=H(4)+H(4)+Q*XI(2);                                              
  DZ=H(2)+H(2)+Q*XI(3);                                              
  %*****TRANSFORM BACK TO GEOMAGNETIC CO-ORDINATE SYSTEM                  
  DXM=IGRF.U(1,1)*DX+IGRF.U(2,1)*DY+IGRF.U(3,1)*DZ;                                 
  DYM=IGRF.U(1,2)*DX+IGRF.U(2,2)*DY;                                           
  DZM=IGRF.U(1,3)*DX+IGRF.U(2,3)*DY+IGRF.U(3,3)*DZ;                                 
  DR=(XM*DXM+YM*DYM+ZM*DZM)/R;                                       
  %*****FORM SLOWLY VARYING EXPRESSIONS                                   
  P(4,k)=(WR*DXM-0.5*P(1,k)*DR)/(R*DZM);                                 
  P(5,k)=(WR*DYM-0.5*P(2,k)*DR)/(R*DZM);                                 
  DSQ=RQ*(DXM*DXM+DYM*DYM+DZM*DZM);
  BQ=DSQ*RQ*RQ;
  P(6,k)=sqrt(DSQ/(RQ+3.*ZM*ZM));                                 
  P(7,k)=P(6,k)*(RQ+ZM*ZM)/(RQ*DZM);                                     

end

