

function store_image2Hdf5(figurefile,hdf5file)
display(figurefile)
[X,map] = imread(figurefile);
figinfo = imfinfo(figurefile);
F = fieldnames(figinfo);

[~,figurename, ext] = fileparts(figurefile);
figurename = [figurename ext];

for ii = 1:length(F)
    if ~strcmp(F{ii},'Filename') && ~strcmp(F{ii},'Description') && ~strcmp(F{ii},'Title') && ~strcmp(F{ii},'Copyright') && ~strcmp(F{ii},'Author') && ~strcmp(F{ii},'Comment') && ~strcmp(F{ii},'Source') 
        figinfo = rmfield(figinfo,F{ii});
    elseif isempty(figinfo.(char(F{ii})))
        figinfo.(char(F{ii})) = '';   
    end
end

if isfield(figinfo,'Filename'),    figinfo = renameStructField(figinfo,'Filename','Figurename');    
    figinfo.Figurename = figurename;
end

if isfield(figinfo,'Description'), figinfo = renameStructField(figinfo,'Description','Experiment'); end
if isfield(figinfo,'Title'),       figinfo = renameStructField(figinfo,'Title','Radar');            end
if isfield(figinfo,'Author'),      figinfo = renameStructField(figinfo,'Author','Computer');        end
if isfield(figinfo,'Comment'),     figinfo = renameStructField(figinfo,'Comment','Results');        end

hdf5write(hdf5file,['/figures' '/' figurename '/imagedata'],X,'WriteMode','append');
hdf5write(hdf5file,['/figures' '/' figurename '/colormap'],map,'WriteMode','append');
hdf5write(hdf5file,['/figures' '/' figurename '/imagemeta'],figinfo','WriteMode','append');
%imwrite(A,map,'test13.hdf5','hdf','compression','rle')



