function [ VTST7,context ] = VTST7( context, IYD,SEC,GLAT,GLONG,STL,F107A,F107,AP,IC )
%VTST7 Test if geophysical variables or switches changed and save
%      FUNCTION VTST7(IYD,SEC,GLAT,GLONG,STL,F107A,F107,AP,IC)
%-----------------------------------------------------------------------
%       Test if geophysical variables or switches changed and save
%       Return 0 if unchanged and 1 if changed
%-----------------------------------------------------------------------

%      DIMENSION AP(7),IYDL(2),SECL(2),GLATL(2),GLL(2),STLL(2)
%      DIMENSION FAL(2),FL(2),APL(7,2),SWL(25,2),SWCL(25,2)
%      COMMON/CSW/SW(25),ISW,SWC(25)
%      SAVE
  VTST7=0;
  if (IYD ~= context.IYDL(IC)) || (SEC ~= context.SECL(IC)) || ...
     (GLAT ~= context.GLATL(IC)) || ...
     (GLONG ~= context.GLL(IC)) || (STL ~= context.STLL(IC)) || ...
     (F107A ~= context.FAL(IC)) || (F107 ~= context.FL(IC))
    VTST7 = 1;
  end
  for I=1:CIRA.maxAP
    if AP(I) ~= context.APL(I,IC)
      VTST7 = 1;
      break;
    end
  end
  for I=1:CIRA.maxSW
    if context.SW(I) ~= context.SWL(I,IC)
      VTST7 = 1;
      break;
    end
    if context.SWC(I) ~= context.SWCL(I,IC)
      VTST7 = 1;
      break;
    end
  end
  if VTST7 == 1
    context.IYDL(IC)=IYD;
    context.SECL(IC)=SEC;
    context.GLATL(IC)=GLAT;
    context.GLL(IC)=GLONG;
    context.STLL(IC)=STL;
    context.FAL(IC)=F107A;
    context.FL(IC)=F107;
    for I=1:CIRA.maxAP
      context.APL(I,IC)=AP(I);
    end
    for I=1:CIRA.maxSW
      context.SWL(I,IC)=context.SW(I);
      context.SWCL(I,IC)=context.SWC(I);
    end
  end
end

