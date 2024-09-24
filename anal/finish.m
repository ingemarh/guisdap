function finish()
global local
if isstruct(local)
  [~]=rmdir(local.tfile,'s');
  delete([local.tfile '*'])
end
setuplibs(1)
