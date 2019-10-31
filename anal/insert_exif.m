function insert_exif(fig,file,exts)
exif=''; td=[];
prog='exiftool';
flags='-overwrite_original';
ud={'Experiment' 'Radar' 'Copyright' 'Computer' 'Results'};
fd={'Name'};
d=findobj(fig,'type','axes');
for i=d'
  t=get(i,'xlim');
  if t(1)>7e5 && t(2)<8e5
    td=(t-datenum(1970,1,1))*86400;
  end
end
if unix(['which ' prog ' >/dev/null'])
  warning('GUISDAP:vizu',[prog ' not found, please install'])
  prog='imwrite';
  f=imformats; lf=length(f); j=0;
  for i=1:lf
    e=intersect(exts,f(i).ext);
    if ~isempty(e)
      j=j+1;ex(j)=e;
    end
  end
  if j
    exts=ex;
  else
    return
  end
end
if strcmp(prog,'exiv2')
  tags={' -M"set Exif.Image.ImageDescription ' ' -M"set Exif.Image.DocumentName ' ' -M"set Exif.Image.Copyright ' ' -"set Exif.Image.HostComputer ' ' -M"set Exif.Photo.UserComment '};
  ftags={' -M"set Exif.Image.ImageID '};
  exif=cmdline(fig,ud,tags,td,fd,ftags,'"');
elseif strcmp(prog,'exiftool')
  tags={' -description="' ' -title="' ' -copyright="' ' -author="' ' -comment="'};
  ftags={' -source="'};
  exif=cmdline(fig,ud,tags,td,fd,ftags,'"');
elseif strcmp(prog,'imwrite')
  tags={'Description' 'Title' 'Copyright' 'Author' 'Comment'};
  ftags={'Source'};
  exif=cmdline(fig,ud,tags,td,fd,ftags,[]);
end
if ~isempty(exif)
  for ext=exts
     if strcmp(prog,'imwrite')
      try
       [x,map]=imread(file,char(ext));
       ex=''; for f=fieldnames(exif)', ex=[ex ',''' char(f) ''', getfield(exif,''' char(f) ''')' ]; end
       f=[file '.' char(ext)];
       eval(['imwrite(x,map,f' ex ')']);
      catch
       disp(lasterror)
      end
     else
      [i,i]=unix(sprintf('%s %s %s %s.%s',prog,flags,exif,file,char(ext)));
     end
  end
end
return

function cmd=cmdline(fig,ud,tag,td,fd,ftag,tail)
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
   if isempty(tail)
    cmd=setfield(cmd,char(tag(f)),ss(1:end-2));
   else
    cmd=[cmd char(tag(f)) ss(1:end-2) tail];
   end
  else
   if f==1 && ~isempty(td), s=sprintf('%s [%.0f %.0f]',s,td); end
   if isempty(tail)
    cmd=setfield(cmd,char(tag(f)),s);
   else
    cmd=[cmd char(tag(f)) s tail];
   end
  end
 end
end
for f=1:length(fd)
  i=get(fig,char(fd(f)));
  if ~isempty(i)
   if isempty(tail)
    cmd=setfield(cmd,char(ftag(f)),i);
   else
    cmd=[cmd char(ftag(f)) i tail];
   end
  end
end
return
