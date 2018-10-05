function D = MixingCorrection(context,DM,DZ28,DS,DMC,massI)
  % correction for mixing at lower altitudes
  % INPUT:
  %   DM - mix density (cm^-3)
  %   DZ28 - N2 density (cm^-3)
  %   DS - pre-mix density (cm^-3)
  %   DMC - ratio
  % OUTPUT:
  %   D - mixed density (cm^-3)
  pdm = context.PDM(2,massI);
  D = zeros(1,CIRA.DerLast);
  if pdm < 0
    % already zeros
  elseif DMC(1,CIRA.Der0) == 0 && DMC(1,CIRA.DerAlt) == 0
    for d=1:CIRA.DerLast
      D(1,d)=DM(1,d)*pdm;
    end
  elseif pdm ~= 0 && DZ28(1,CIRA.Der0) ~= 0
    DMR=DS(1,CIRA.Der0)/(DZ28(1,CIRA.Der0)*pdm)-1.;
    D(1,CIRA.Der0)=DM(1,CIRA.Der0)*pdm*(1.+DMR*DMC(1,CIRA.Der0));
    for d=CIRA.Der0+1:CIRA.DerLast
      DMRd=DS(1,d)/(DZ28(1,CIRA.Der0)*pdm) ...
        -DS(1,CIRA.Der0)*DZ28(1,d)/(DZ28(1,CIRA.Der0)*pdm)/DZ28(1,CIRA.Der0);
      D(1,d)=DM(1,d) ...
        *pdm*(1.+DMR*DMC(1,CIRA.Der0)) ...
        +DM(1,CIRA.Der0)*pdm*(DMRd*DMC(1,CIRA.Der0)+DMR*DMC(1,d));
    end
  elseif DZ28(1,CIRA.Der0) ~= 0 % && pdm == 0
    DMR=DS(1,CIRA.Der0)/DZ28(1,CIRA.Der0);
    D(1,CIRA.Der0)=DM(1,CIRA.Der0)*(DMR*DMC(1,CIRA.Der0));
    for d=CIRA.Der0+1:CIRA.DerLast
      DMRd=DS(1,d)/DZ28(1,CIRA.Der0) ...
        -DS(1,CIRA.Der0)*DZ28(1,d)/DZ28(1,CIRA.Der0)/DZ28(1,CIRA.Der0);
      D(1,d)=DM(1,d) ...
        *(DMR*DMC(1,CIRA.Der0)) ...
        +DM(1,CIRA.Der0)*(DMRd*DMC(1,CIRA.Der0)+DMR*DMC(1,d));
    end
  else % if DZ28 == 0
    DMR=DS(1,CIRA.Der0);

    if DM(1,CIRA.Der0) == 0 || DMC(1,CIRA.Der0) == 0 % prevent NaN
      D(1,CIRA.Der0)=(DMR*DMC(1,CIRA.Der0));
      if DMC(1,CIRA.Der0) == 0 % prevent NaN
        for d=CIRA.Der0+1:CIRA.DerLast
          D(1,d)=-DS(1,CIRA.Der0)*DZ28(1,d);
        end
      else % infinity
        for d=CIRA.Der0+1:CIRA.DerLast
          D(1,d)=(-DS(1,CIRA.Der0)*DZ28(1,d)*DMC(1,CIRA.Der0))/DZ28(1,CIRA.Der0);
        end
      end
    else % infinity
      D(1,CIRA.Der0)=DM(1,CIRA.Der0)*(DMR*DMC(1,CIRA.Der0))/DZ28(1,CIRA.Der0);
      for d=CIRA.Der0+1:CIRA.DerLast
        D(1,d)=DM(1,CIRA.Der0)*(-DS(1,CIRA.Der0)*DZ28(1,d)*DMC(1,CIRA.Der0)) ...
          /DZ28(1,CIRA.Der0)/DZ28(1,CIRA.Der0);
      end
    end
  end
end
