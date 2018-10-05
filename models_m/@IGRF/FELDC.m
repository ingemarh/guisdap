function [ B ] = FELDC( context, V )
%FELDC ENTRY POINT  FELDC  TO BE USED WITH CARTESIAN CO-ORDINATES

%*****ENTRY POINT  FELDC  TO BE USED WITH CARTESIAN CO-ORDINATES        
%      ENTRY FELDC(V,B)                                                  
  XXX=V(1);
  YYY=V(2);
  ZZZ=V(3);
  RQ=1./(XXX*XXX+YYY*YYY+ZZZ*ZZZ);
  [~,B] = context.FELDI( RQ, XXX, YYY, ZZZ, 2 );

end

