function [ XE5,dXE5 ] = XE5( context, H )
%XE5 ELECTRON DENSITY FOR THE E AND VALLEY REGION
%        REAL FUNCTION XE5(H)                         
% ELECTRON DENSITY FOR THE E AND VALLEY REGION (HME..HEF).   

%        LOGICAL NIGHT   
%        COMMON    /BLOCK4/        HME,XNME,HEF
%     &          /BLOCK5/        NIGHT,E(4)                    
  T3=H-context.HME;
  dT3=1.0;
  T1=T3*T3*(context.E(1)+T3*(context.E(2) ...
       +T3*(context.E(3)+T3*context.E(4))));
  dT1=dT3*T3*(2*context.E(1)+T3*(3*context.E(2) ...
       +T3*(4*context.E(3)+T3*5*context.E(4))));
  if ~context.ENIGHT
    XE5=context.NME*(1+T1);
    dXE5=context.NME*dT1;
  else
    XE5=context.NME*exp(T1);
    dXE5=XE5*dT1;
  end

end

