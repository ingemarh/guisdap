function [D,T] = SetOutput(context,D,T,DZ,TZ,massI)
  % set the output array of densities for a particular constituent
  % This function adds in the solar time dependency on time and longitude to the derivatives
  STLdLon = CIRA.DGTR/context.HR;
  STLdSec = context.SR/context.HR;
  if length(DZ) > 1
    for d=CIRA.Der0:CIRA.DerLast
      if d ~= CIRA.DerSTL
        D(CIRA.DensityIndex(d,massI)) = DZ(1,d);
      else
        D(CIRA.DensityIndex(CIRA.DerSec,massI)) = D(CIRA.DensityIndex(CIRA.DerSec,massI)) + DZ(1,d)*STLdSec;
        D(CIRA.DensityIndex(CIRA.DerLon,massI)) = D(CIRA.DensityIndex(CIRA.DerLon,massI)) + DZ(1,d)*STLdLon;
      end
    end
  else
    D(CIRA.DensityIndex(CIRA.Der0,massI)) = DZ;
    for d=CIRA.Der0+1:CIRA.DerLast
      if d ~= CIRA.DerSTL
        D(CIRA.DensityIndex(d,massI)) = 0;
      end
    end
  end
  if length(TZ) > 1
    for d=CIRA.Der0:CIRA.DerLast
      if d ~= CIRA.DerSTL
        T(CIRA.TemperatureIndex(d)) = TZ(1,d);
      else
        T(CIRA.TemperatureIndex(CIRA.DerSec)) = T(CIRA.TemperatureIndex(CIRA.DerSec)) + TZ(1,d)*STLdSec;
        T(CIRA.TemperatureIndex(CIRA.DerLon)) = T(CIRA.TemperatureIndex(CIRA.DerLon)) + TZ(1,d)*STLdLon;
      end
    end
  else
    T(CIRA.TemperatureIndex(CIRA.Der0)) = TZ;
    for d=CIRA.Der0+1:CIRA.DerLast
      if d ~= CIRA.DerSTL
        T(CIRA.TemperatureIndex(d)) = 0.0;
      end
    end
  end
end
