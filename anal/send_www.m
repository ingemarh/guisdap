function send_www
global webfile
l=length(webfile);
if l>0 & isunix
 [i,d]=unix('ps | grep curl | grep -v grep');
 if i
  file=[];
  for i=1:l
   if iscellstr(webfile(i))
    file=[file ' -F file=@' char(webfile(i))];
   end
  end
  unix(['curl -s' file ' "http://www.eiscat.com/raw/rtg/upload.cgi" >/dev/null &']);
 end
end
