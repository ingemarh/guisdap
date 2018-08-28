function [ scaleh,SCALHdZ,SCALHdRE,SCALHdGS,SCALHdM,SCALHdT ] = SCALH( ALT,RE,GSURF,XM,TEMP )
%SCALH Calculate scale height (km)
%      FUNCTION SCALH(ALT,XM,TEMP)
%-----------------------------------------------------------------------
%      Calculate scale height (km)
%-----------------------------------------------------------------------

%      COMMON/PARMB/GSURF,RE
%      SAVE
  G=GSURF/(1.+ALT/RE)^2;
  scaleh=CIRA.RGAS*TEMP/(G*XM);
  if nargout > 1
    GdZ=-2*G/(RE+ALT);
    GdRE=2*G*(ALT/RE)/(RE+ALT);
    GdGS=1/(1.+ALT/RE)^2;
    SCALHdZ=-scaleh*GdZ/G;
    SCALHdRE=-scaleh*GdRE/G;
    SCALHdGS=-scaleh*GdGS/G;
    SCALHdM=-scaleh/XM;
    SCALHdT=CIRA.RGAS/(G*XM);
  end

end

