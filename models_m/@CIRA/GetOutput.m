function [ DZ ] = GetOutput( D, massI )
%GETOUTPUT retrieves the density from the output array
  DZ = zeros(1,CIRA.DerLast);
  for d=CIRA.Der0:CIRA.DerLast
    if d ~= CIRA.DerSTL
      DZ(1,d)=D(CIRA.DensityIndex(d,massI));
    end
  end

end

