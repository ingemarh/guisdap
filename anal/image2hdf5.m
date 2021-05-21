
function image2hdf5(figurefile,hdf5file)
% function image2hdf5(figurefile,hdf5file)
% store data from an image (figurefile) to and HDF5-file (hdf5file)

if nargin<2 
    error('A figure file and an HDF5 file to save the figure data to are needed as input');
end

warning('off','MATLAB:imagesci:png:libraryWarning')
try
    [X,map] = imread(figurefile);    
catch
    display(['Failed to read ' figurefile '.'])
    return
end
if ~isempty(map)
    x = round(uint8(255*ind2rgb(X,map)));
else
    x = X;
end

figinfo = imfinfo(figurefile);
F = fieldnames(figinfo);

[~,figurename, ext] = fileparts(figurefile);
figurename = [figurename ext];

for ii = 1:length(F)
    if ~contains('Filename Description Title Copyright Author Comment Source',F{ii}) || isempty(figinfo.(F{ii}))
        figinfo = rmfield(figinfo,F{ii});
    end
end

if isfield(figinfo,'Filename'),    figinfo = renameStructField(figinfo,'Filename','Figurename'); end
figinfo.Figurename = figurename;

% Renaming some of the figure metadata fields
FieldnamesToChange = {'Description','Title','Author','Comment'};
FieldnamesChangeTo = {'Experiment','Radar','Computer','Results'};
for fn = 1: length(FieldnamesToChange)
    if isfield(figinfo,FieldnamesToChange{fn})
        figinfo = renameStructField(figinfo,FieldnamesToChange{fn},FieldnamesChangeTo{fn});
    end
end

chunklim = 1024;
if ~isempty(x)
    sx=size(x);
    %csize = sx;
    %csize(find(csize>chunklim)) = chunklim;
    Desc(1) = {sprintf('imagedata: RGB truecolor image data, an %d-by-%d-by-%d array.',sx)};
    %h5create(hdf5file,['/figures/' figurename '/imagedata'],sx,'ChunkSize',csize,'Deflate',9,'Datatype','uint8');
    %h5write(hdf5file, ['/figures/' figurename '/imagedata'],x);
    strds2hdf5(hdf5file,['/figures/' figurename],'imagedata',x)
end

figfields = fieldnames(figinfo);
for ii = 1:length(figfields)
    strds2hdf5(hdf5file,['/figures/' figurename '/imagemeta'],figfields{ii},{figinfo.(figfields{ii})})
end
strds2hdf5(hdf5file,['/figures/' figurename],'DataDescription',Desc)
