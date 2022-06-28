function send_www
global webfile name_site
l=length(webfile);
if l>0 & isunix
 [i,d]=unix('ps | grep curl | grep -v grep');
 if i
  file=[];
  for i=1:l
   if iscellstr(webfile(i))
    file2=char(webfile(1));       % For local web server
    file=[file ' -F file=@' char(webfile(i))];
   end
  end
  unix(['LD_LIBRARY_PATH="" && curl -s' file ' "https://portal.eiscat.se/rtg/upload.cgi" >/dev/null &']);
  if exist('file2','var') & (name_site=='T' | name_site=='V'), unix(['scp ' file2 ' palver5:/var/www/html/rtg/ >/dev/null &']); end
 end
end
