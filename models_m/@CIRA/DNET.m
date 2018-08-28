function [ dnet,DNETdDD,DNETdDM ] = DNET( DD,DM,ZHM,XMM,XM )
%DNET TURBOPAUSE CORRECTION FOR MSIS MODELS
%      FUNCTION DNET(DD,DM,ZHM,XMM,XM)
%-----------------------------------------------------------------------
%       TURBOPAUSE CORRECTION FOR MSIS MODELS
%         DNET=(DD^A+DM^A)^(1.0/A);
%         Root mean density
%       8/20/80
%          DD - diffusive density
%          DM - full mixed density
%          ZHM - transition scale length
%          XMM - full mixed molecular weight
%          XM  - species molecular weight
%          DNET - combined density
%-----------------------------------------------------------------------
  if XMM == XM
    fprintf(1,'DNET EXPONENT ERROR %g %g %g\n',ZHM,XMM,XM);
    if ZHM < 0.
      if abs(DD) > abs(DM)
        dnet=DM;
        DNETdDM=1.0;
        DNETdDD=0;
      else
        dnet=DD;
        DNETdDM=1.0;
        DNETdDD=0;
      end
    elseif ZHM > 0.
      if abs(DD) > abs(DM)
        dnet=DD;
        DNETdDD=1.0;
        DNETdDM=0;
      else
        dnet=DM;
        DNETdDM=1.0;
        DNETdDD=0;
      end
    else % A = 0/0 (assume A=1)
      dnet=DD+DM;
      DNETdDD=1.0;
      DNETdDM=1.0;
    end
  elseif ZHM == 0
    fprintf(1,'DNET EXP ERROR %g %g %g\n',ZHM,XMM,XM);
    if abs(DM) > abs(DD)
      dnet=DM;
      DNETdDM=1.0;
      DNETdDD=0.0;
    elseif abs(DM) < abs(DD)
      dnet=DD;
      DNETdDD=1.0;
      DNETdDM=0.0;
    else
      if DD >= 0
        dnet=Inf;
        DNETdDD=Inf;
        DNETdDM=Inf;
      else
        dnet=-Inf;
        DNETdDD=-Inf;
        DNETdDM=-Inf;
      end
    end
  elseif DM <= 0 || DD <= 0
    fprintf(1,'DNET LOG ERROR %g %g %g\n',DM,DD,XM);
    if DD == 0 && DM == 0
      %DD=1.;
      if ZHM*(XMM-XM) > 0.0
        dnet = 0.0;
        DNETdDD = 0.0; % ?
        DNETdDM = 0.0; % ?
      else
        dnet = Inf; % infinity
        DNETdDD = Inf; % ?
        DNETdDM = Inf; % ?
      end
    elseif DM == 0
      dnet=DD;
      DNETdDD=1.0;
      DNETdDM=0.0;
    elseif DD == 0
      dnet=DM;
      DNETdDD=0.0;
      DNETdDM=1.0;
    else
      A = ZHM/(XMM-XM);
      dnet=(DD^A+DM^A)^(1.0/A); % DD < 0 and DM < 0
      DNETdDD=(DD^(A-1.0))*(DD^A+DM^A)^(1.0/A-1.0);
      DNETdDM=(DM^(A-1.0))*(DD^A+DM^A)^(1.0/A-1.0);
    end
  else
    lim = 10.0;
    A = ZHM/(XMM-XM);
    YLOG=A*log(DM/DD);
    if YLOG < -lim % DD >>> DM
      dnet=DD;
      DNETdDD=1.0;
      DNETdDM=0.0;
    elseif YLOG > lim % DM >>> DD
      dnet=DM;
      DNETdDD=0.0;
      DNETdDM=1.0;
    elseif YLOG < 0
      EXP = exp(YLOG);
      EXP1=(1.+EXP)^(1.0/A);
      EXP2=EXP1/(1.+EXP);
      dnet=DD*EXP1;
      DNETdDD=EXP1-EXP2*EXP;
      DNETdDM=(DD/DM)*EXP2*EXP;
    else
      EXP = exp(-YLOG);
      EXP1=(1.+EXP)^(1.0/A);
      EXP2=EXP1/(1.+EXP);
      dnet=DM*EXP1;
      DNETdDM=EXP1-EXP2*EXP;
      DNETdDD=(DM/DD)*EXP2*EXP;
    end
  end
end

