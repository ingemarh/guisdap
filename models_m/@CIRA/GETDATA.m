function [ context ] = GETDATA( context, FN )
%GETDATA reads the data file and populates the instance
  persistent fnMap;
  if isempty(fnMap)
    fnMap = containers.Map('KeyType','char','ValueType','any');
  end
  if isKey(fnMap,FN)
    sc = fnMap(FN);
  else
    fp = fopen(FN,'r');
    if fp ~= -1
      sc1 = textscan(fp,'%s',3);
      sc2 = textscan(fp,'%d',1);
      sc3 = textscan(fp,'%s',sc2{1}(1));
      sc = textscan(fp,'%f');
      sc{2} = sc1{1};
      sc{3} = sc2{1};
      sc{4} = sc3{1};
      fclose(fp);
      fnMap(FN) = sc;
    else
      return;
    end
  end
  
  context.NAM = sc{2}(1);
  context.ISD = sc{2}(2);
  context.IST = sc{2}(3);
  k = 1;
  NumMasses = length(sc{4});
  context.constituentName = cell(NumMasses,1);
  if length(sc{4}) ~= NumMasses
    printf('WARNING: bad number of consituent names\n');
  end
  for i=1:NumMasses
    context.constituentName{i} = sc{4}(i);
  end
  N = sc{1}(k); k = k + 1;
  if N ~= NumMasses
    printf('WARNING: bad number of DISASSOCIATED\n');
  end
  context.DISASSOCIATED = zeros(N,1);
  for i=1:N
    context.DISASSOCIATED(i) = sc{1}(k); k = k + 1;
  end
  N = sc{1}(k); k = k + 1;
  if N ~= NumMasses
    printf('WARNING: bad number of MT\n');
  end
  context.MT = zeros(N,1);
  for i=1:N
    context.MT(i) = sc{1}(k); k = k + 1;
  end
  N = sc{1}(k); k = k + 1;
  if N ~= NumMasses
    printf('WARNING: bad number of ALPHA\n');
  end
  context.ALPHA = zeros(N,1);
  for i=1:N
    context.ALPHA(i) = sc{1}(k); k = k + 1;
  end
  N = sc{1}(k); k = k + 1;
  if N ~= NumMasses
    printf('WARNING: bad number of ALTL\n');
  end
  context.ALTL = zeros(N,1);
  for i=1:N
    context.ALTL(i) = sc{1}(k); k = k + 1;
  end
  N = sc{1}(k); k = k + 1;
  M = sc{1}(k); k = k + 1;
  if M ~= NumMasses
    printf('WARNING: bad number of columns in MIXINDEX\n');
  end
  context.MIXINDEX = zeros(N,M);
  for j=1:M
    for i=1:N
      context.MIXINDEX(i,j) = sc{1}(k); k = k + 1;
    end
  end
  N = sc{1}(k); k = k + 1;
  M = sc{1}(k); k = k + 1;
  if M ~= NumMasses
    printf('WARNING: bad number of columns in EQUILIBINDEX\n');
  end
  context.EQUILIBINDEX = zeros(N,M);
  for j=1:M
    for i=1:N
      context.EQUILIBINDEX(i,j) = sc{1}(k); k = k + 1;
    end
  end
  context.defaultF107 = sc{1}(k); k = k + 1;
  context.defaultAp = sc{1}(k); k = k + 1;
  context.defaultLatitude = sc{1}(k); k = k + 1;
  context.ZMIX = sc{1}(k); k = k + 1;
  context.TINFLim = sc{1}(k); k = k + 1;
  context.LTHHT = sc{1}(k); k = k + 1;
  context.DR = 2*CIRA.pi/sc{1}(k); k = k + 1;
  context.SR = 2*CIRA.pi/sc{1}(k); k = k + 1;
  context.HR = 2*CIRA.pi/sc{1}(k); k = k + 1;
  context.mainConstituent = sc{1}(k); k = k + 1;
  context.REQ = sc{1}(k); k = k + 1;
  context.FI = sc{1}(k); k = k + 1;
  context.GPL = sc{1}(k); k = k + 1;
  context.GEQ = sc{1}(k); k = k + 1;
  N = sc{1}(k); k = k + 1;
  context.ZN1 = zeros(N,1);
  for i=1:N
    context.ZN1(i) = sc{1}(k); k = k + 1;
  end
  context.MN1 = N;
  N = sc{1}(k); k = k + 1;
  if N ~= context.MN1
    printf('WARNING: bad number of columns in PTMINDEX\n');
  end
  context.PTMINDEX = zeros(N,1);
  for i=1:N
    context.PTMINDEX(i) = sc{1}(k); k = k + 1;
  end
  N = sc{1}(k); k = k + 1;
  context.ZN2 = zeros(N,1);
  for i=1:N
    context.ZN2(i) = sc{1}(k); k = k + 1;
  end
  context.MN2 = N;
  N = sc{1}(k); k = k + 1;
  context.ZN3 = zeros(N,1);
  for i=1:N
    context.ZN3(i) = sc{1}(k); k = k + 1;
  end
  context.MN3 = N;
  N = sc{1}(k); k = k + 1;
  context.p = zeros(N,1);
  for i=1:N
    context.p(i) = sc{1}(k); k = k + 1;
  end
  N = sc{1}(k); k = k + 1;
  context.g = zeros(N,1);
  for i=1:N
    context.g(i) = sc{1}(k); k = k + 1;
  end
  N = sc{1}(k); k = k + 1;
  context.PTM = zeros(N,1);
  for i=1:N
    context.PTM(i) = sc{1}(k); k = k + 1;
  end
  N = sc{1}(k); k = k + 1;
  M = sc{1}(k); k = k + 1;
  if M ~= NumMasses
    printf('WARNING: bad number of columns in PDM\n');
  end
  context.PDM = zeros(N,M);
  for j=1:M
    for i=1:N
      context.PDM(i,j) = sc{1}(k); k = k + 1;
    end
  end
  N = sc{1}(k); k = k + 1;
  context.PT = zeros(N,1);
  for i=1:N
    context.PT(i) = sc{1}(k); k = k + 1;
  end
  M = sc{1}(k); k = k + 1;
  N = sc{1}(k); k = k + 1;
  if M ~= NumMasses
    printf('WARNING: bad number of columns in PDA\n');
  end
  context.PDA = zeros(N,M);
  for m=1:M
    for i=1:N
      context.PDA(i,m) = sc{1}(k); k = k + 1;
    end
  end
  N = sc{1}(k); k = k + 1;
  context.PS = zeros(N,1);
  for i=1:N
    context.PS(i) = sc{1}(k); k = k + 1;
  end
  N = sc{1}(k); k = k + 1;
  M = sc{1}(k); k = k + 1;
  context.PDL = zeros(N,M);
  for j=1:M
    for i=1:N
      context.PDL(i,j) = sc{1}(k); k = k + 1;
    end
  end
  N = sc{1}(k); k = k + 1;
  M = sc{1}(k); k = k + 1;
  context.PTL = zeros(N,M);
  for j=1:M
    for i=1:N
      context.PTL(i,j) = sc{1}(k); k = k + 1;
    end
  end
  N = sc{1}(k); k = k + 1;
  M = sc{1}(k); k = k + 1;
  context.PMA = zeros(N,M);
  for j=1:M
    for i=1:N
      context.PMA(i,j) = sc{1}(k); k = k + 1;
    end
  end
  N = sc{1}(k); k = k + 1;
  context.SAM = zeros(N,1);
  for i=1:N
    context.SAM(i) = sc{1}(k); k = k + 1;
  end
  N = sc{1}(k); k = k + 1;
  context.PAVGM = zeros(N,1);
  for i=1:N
    context.PAVGM(i) = sc{1}(k); k = k + 1;
  end
end

