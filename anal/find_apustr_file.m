function [file,apu]=find_apustr_file(a,apu,b,e,sub)
file=[];
if nargin>4
  apu=fix(apu);
  f=sprintf('%s_%d.*%s.%s*',a,apu,b,e);
  files=dir(f);
  if ~isempty(files)
    [~,fst]=fileparts(a);
    sv=[];
    for f=files'
      sv=[sv sscanf(f.name,[fst '_' int2str(apu) '.%d*'])];
    end
    appu=apu+sv/10;
    disp(['Found subversions ' sprintf('%g ', appu)])
    for apu=appu
      if contains(sub,num2str(apu))
        break
      end
    end
  end
else
  f=sprintf('%s_%g%s.%s',a,apu,b,e);
  if exist(f)==2 | exist([f '.gz'])==2 | exist([f '.bz2'])==2
    file=sprintf('%s_%g%s',a,apu,b);
  elseif fix(apu)~=apu
    apu=fix(apu);
    f=sprintf('%s_%d%s.%s',a,fix(apu),b,e);
    if exist(f)==2 | exist([f '.gz'])==2 | exist([f '.bz2'])==2
      file=sprintf('%s_%d%s',a,apu,b);
    end
  end
  if isempty(file)
    f=sprintf('%s%s.%s',a,b,e);
    if exist(f)==2 | exist([f '.gz'])==2 | exist([f '.bz2'])==2
      file=sprintf('%s%s',a,b);
      apu=[];
    end
  end
  if isempty(file)
    warning('GUISDAP:default',['Could not find file: ' f])
  end
end
