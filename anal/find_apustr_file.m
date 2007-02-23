function [file,apu]=find_apustr_file(a,apu,b,e)
if apu(1)~='_'
 apu=['_' apu];
end
if ~isempty(e) & e(1)~='.'
 e=['.' e];
end
if exist([a apu b e])==2
  file=[a apu b];
elseif exist([a b e])==2
  file=[a b];
  apu=[];
elseif exist([a apu(2:end) b e])==2
  file=[a apu(2:end) b];
elseif exist([a '_' b e])==2
  file=[a '_' b];
  apu=[];
else
  file=[]; apu=[];
  warning('GUISDAP:default',['Could not find file: ' a b e])
end
