% Generating a pdf-file from an eps or ps file.

function pdffile = eps2pdf(epsfile) 

if nargin<1
    error('No eps or ps-file as input')
end

[dirpath,epsfilename,ext] = fileparts(epsfile);
if ~strcmp(ext,'.eps') && ~strcmp(ext,'.ps')
    error('The input file is not .eps or .ps')
end

epsfile = [dirpath filesep epsfilename];

pngor = '820x580';
gd=fullfile(matlabroot,'sys','ghostscript',filesep);
gsbin=fullfile(gd,'bin',lower(computer),'gs');
gsinc=sprintf('-I%sps_files -I%sfonts',gd,gd);
if ~exist(gsbin,'file'), gsbin='gs'; gsinc=[]; end
unix(sprintf('%s -I%sps_files -I%sfonts -dNOPAUSE -q -sDEVICE=pdfwrite -sPAPERSIZE=a4 -sOutputFile=%s.pdf %s%s </dev/null >/dev/null',gsbin,gsinc,pngor,epsfile,epsfile,ext));
pdffile = [epsfile '.pdf'];