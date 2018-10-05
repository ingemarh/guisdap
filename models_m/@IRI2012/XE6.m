function [ XE6,dXE6 ] = XE6( context, H )
%XE6 ELECTRON DENSITY FOR THE D REGION
%        REAL FUNCTION XE6(H)                         
% ELECTRON DENSITY FOR THE D REGION (HA...HME).    

%        COMMON    /BLOCK4/        HME,XNME,HEF
%     &          /BLOCK6/        HMD,XNMD,HDX
%     &        /BLOCK7/        D1,XKK,FP30,FP3U,FP1,FP2    
  if H <= context.HDX
    Z=H-context.HMD;
    dZ=1.0;
    if Z > 0.0
      FP3=context.FP30;
    else
      FP3=context.FP3U;
    end
    XE6=context.NMD*exp(Z*(context.FP1+Z*(context.FP2+Z*FP3)));
    dXE6=XE6*(dZ*(context.FP1+Z*(2*context.FP2+Z*3*FP3)));
  else
    Z=context.HME-H;
    dZ=-1.0;
    ZZ = Z^context.XKK;
    if ZZ ~= 0
      XE6=context.NME*exp(-context.D1*ZZ);
      dXE6=XE6*(-context.XKK*context.D1*ZZ/Z*dZ);
    else
      XE6=context.NME;
      dXE6=0.0;
    end
  end
end

