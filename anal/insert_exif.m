function insert_exif(fig,file,exts)
exif='';
prog='exiftool';
if unix(['which ' prog ' >/dev/null'])
  warning('GUISDAP:vizu',[prog ' not found, please install'])
  return
elseif strcmp(prog,'exiv2')
  exif=cmdline(exif,fig,'Experiment',' -M"set Exif.Image.ImageDescription ');
  exif=cmdline(exif,fig,'Radar',' -M"set Exif.Image.DocumentName ');
  exif=cmdline(exif,fig,'Copyright',' -M"set Exif.Image.Copyright ');
  exif=cmdline(exif,fig,'Computer',' -"set Exif.Image.HostComputer ');
  exif=cmdline(exif,fig,'Results',' -M"set Exif.Photo.UserComment ');
  i=get(fig,'Name');
  if ~isempty(i), exif=[exif ' -M"set Exif.Image.ImageID ' i '"']; end
elseif strcmp(prog,'exiftool')
  exif=cmdline(exif,fig,'Experiment',' -description="');
  exif=cmdline(exif,fig,'Radar',' -title="');
  exif=cmdline(exif,fig,'Copyright',' -copyright="');
  exif=cmdline(exif,fig,'Computer',' -author="');
  exif=cmdline(exif,fig,'Results',' -comment="');
  i=get(fig,'Name');
  if ~isempty(i), exif=[exif ' -source="' i '"']; end
end
if ~isempty(exif)
	exif
  for ext=exts
     [i,i]=unix(sprintf('%s %s %s.%s',prog,exif,file,char(ext)));
  end
end
return

function cmd=cmdline(cmd,fig,ud,tag)
i=findobj(fig,'type','text','UserData',ud);
if ~isempty(i), cmd=[cmd tag row(char(get(i,'string'))) '"']; end
if strcmp(ud,'Results')
	i
    keyboard

get(i,'string')
char(get(i,'string')')
row(char(get(i,'string')'))
end
return
