function finish()
global local
if isstruct(local)
  delete([local.tfile '*'])
end
setuplibs(1)
