function save_m(file,varargin)
if isempty(file)
 fid=1;
else
 fid=fopen(file,'w');
 if fid<0, error(['Cannot write to ' file]); end
end
nv=length(varargin);
for i=1:nv
 a=inputname(i+1);
 b=varargin{i};
 [n,m]=size(b);
 fprintf(fid,'%s=',a);
 if iscell(b)
  writecell(fid,b,n,m)
 elseif isstr(b)
  writestr(fid,b,n,m,'\n')
 elseif isstruct(b)
  fprintf(fid,'NaN');
 else
  writedouble(fid,b,n,m,'\n')
 end
 fprintf(fid,';\n');
end
if ~isempty(file)
 fclose(fid);
end

function writecell(fid,b,n,m)
fprintf(fid,'{')
for in=1:n
 for im=1:m
  bb=cell2mat(b(in,im));
  [nn,mm]=size(bb);
  if isstr(bb)
   writestr(fid,bb,nn,mm,';')
  else
   writedouble(fid,bb,nn,mm,';')
  end
  if im~=m
   fprintf(fid,' ');
  end
 end
 if in~=n
  fprintf(fid,'\n ');
 end
end
fprintf(fid,'}');

function writestr(fid,b,n,m,nl)
if n>1
 fprintf(fid,'[');
end
for in=1:n
 bl=b(in,:); d=fliplr(find(bl==''''));
 for l=d, bl=[bl(1:l) '''' bl(l+1:end)]; end
 fprintf(fid,' ''%s''',bl);
 if in~=n
  fprintf(fid,nl);
 end
end
if n>1
 fprintf(fid,']');
end

function writedouble(fid,b,n,m,nl)
if n*m>1
 fprintf(fid,'[');
end
for in=1:n
 for im=1:m
  if imag(b(in,im))
   fprintf(fid,' %g+%gi',real(b(in,im)),imag(b(in,im)));
  else
   fprintf(fid,' %g',b(in,im));
  end
 end
 if in~=n
  fprintf(fid,nl);
 end
end
if n*m>1
 fprintf(fid,']');
end
