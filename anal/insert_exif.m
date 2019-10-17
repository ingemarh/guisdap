function insert_exif(fig,file,exts)
exif='';
prog='exiftool';
ud={'Experiment' 'Radar' 'Copyright' 'Computer' 'Results'};
if unix(['which ' prog ' >/dev/null'])
  warning('GUISDAP:vizu',[prog ' not found, please install'])
  return
elseif strcmp(prog,'exiv2')
  tags={' -M"set Exif.Image.ImageDescription ' ' -M"set Exif.Image.DocumentName ' ' -M"set Exif.Image.Copyright ' ' -"set Exif.Image.HostComputer ' ' -M"set Exif.Photo.UserComment '};
  exif=cmdline(fig,ud,tags);
  i=get(fig,'Name');
  if ~isempty(i), exif=[exif ' -M"set Exif.Image.ImageID ' i '"']; end
elseif strcmp(prog,'exiftool')
  tags={' -description="' ' -title="' ' -copyright="' ' -author="' ' -comment="'};
  exif=cmdline(fig,ud,tags);
  i=get(fig,'Name');
  if ~isempty(i), exif=[exif ' -source="' i '"']; end
end
if ~isempty(exif)
  for ext=exts
     [i,i]=unix(sprintf('%s %s %s.%s',prog,exif,file,char(ext)));
  end
end
return

function cmd=cmdline(fig,ud,tag)
cmd='';
for f=1:length(ud)
 i=findobj(fig,'type','text','UserData',char(ud(f)));
 if ~isempty(i)
  s=get(i,'string');
  if iscell(s)
   ss='';
   for l=s'
    if length(char(l))>2, ss=[ss char(l) newline]; end
   end
   cmd=[cmd char(tag(f)) ss(1:end-2) '"'];
  else
   cmd=[cmd char(tag(f)) get(i,'string') '"'];
  end
 end
end
return
