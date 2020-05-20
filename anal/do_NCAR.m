% do_NCAR.m: function to make NCAR file(s) from results
% GUISDAP v.1.81 03-02-27 Copyright EISCAT
%
%function do_NCAR(path,ascbin,name_ant)
% path=directory holding the files (needs to be writable)
% ascbin=bitpattern bit0 write ascii
%                   bit1 write binary (big-endian)
% ant='32m'|'42m',...
% default values: (result_path,3,name_ant)
% filenames constructed from date,name_expr,name_ant
function do_NCAR(path,ascbin,ant)
global result_path name_expr

if nargin<3, ant=[]; end
if nargin<2, ascbin=[]; end
if nargin<1, path=[]; end
if isempty(path), path=result_path; end
if isempty(ascbin), ascbin=3; end

path=fullfile(path,filesep);
list=getfilelist(path);
if isempty(list), error('No result files found!'), end
load(canon(list(1).fname,0))
if isempty(ant)
 if exist('name_ant','var')
  ant=name_ant;
 else
  ant=path(end-3:end-1);
 end
end

file0=sprintf('%sNCAR_%d-%02d-%02d_%s',path,r_time(2,1:3),name_expr);
d=dir([file0 '*@' ant '.*']);
if length(d)
 file0=[path strtok(d(1).name,'.')];
else
 file0=[file0 '@' ant];
end
i1=[]; i2=[];
if rem(ascbin,2), i1=[file0 '.asc']; end
if ascbin>1, i2=[file0 '.bin']; end
for i=list'
 NCAR_output(canon(i.fname,0),i1,i2)
end
NCAR_output
