function send_www
global webfile name_site local
l=length(webfile);
if l>0 & isunix
 if ~isempty(local.fn.curl)
  file=[];
  for i=1:l
   if iscellstr(webfile(i))
    file2=char(webfile(1));       % For local web server
    file=[file ' -F file=@' char(webfile(i))];
   end
  end
  gupsystem(['curl -s' file ' "https://portal.eiscat.se/rtg/upload.cgi" >/dev/null &']);
  if exist('file2','var') & (name_site=='T' | name_site=='V'), gupsystem(['scp ' file2 ' palver5:/var/www/html/rtg/ >/dev/null &']); end
 end
end
