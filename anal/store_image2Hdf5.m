

function store_image2Hdf5(figurefile,hdf5file)

if nargin<2 
    error('A figure file and an HDF5 file to save the figure data to are needed as input');
end
[X,map] = imread(figurefile);
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

chunklim = 100;
if ~isempty(X)
    Xsize = size(X); 
    nrow = Xsize(1);
    ncol = Xsize(2);
    if ge(nrow,chunklim) && ge(ncol,chunklim), csize = [chunklim chunklim];
    elseif ge(nrow,chunklim), csize = [chunklim ncol];
    elseif ge(ncol,chunklim), csize = [nrow chunklim];
    else, csize = [nrow ncol]; 
    end   
    if length(Xsize)==2
        Desc(1) = {['imagedata: indexed image data, an ' num2str(nrow) '-by-' num2str(ncol) ' array of index values corresponding to the color at that index in "colormap".']};
    elseif length(Xsize)==3
        ndepth = Xsize(3);
        csize = [csize ndepth];
        Desc(1) = {['imagedata: truecolor image data, an ' num2str(nrow) '-by-' num2str(ncol) '-by-3 array.']};
    end
    h5create(hdf5file,['/figures' '/' figurename '/imagedata'],size(X),'ChunkSize',csize,'Deflate',9);
    h5write(hdf5file, ['/figures' '/' figurename '/imagedata'],X);
    
end
if ~isempty(map)
    mapsize = size(map); 
    nrow = mapsize(1);
    ncol = mapsize(2);
    if ge(nrow,chunklim) && ge(ncol,chunklim), csize = [chunklim chunklim];
    elseif ge(nrow,chunklim), csize = [chunklim ncol];
    elseif ge(ncol,chunklim), csize = [nrow chunklim];
    else, csize = [nrow ncol];
    end
    if length(mapsize)==3
        ndepth = mapsize(3);
        csize = [csize ndepth];
    end
    h5create(hdf5file,['/figures' '/' figurename '/colormap'],size(map),'ChunkSize',csize,'Deflate',9);
    h5write(hdf5file, ['/figures' '/' figurename '/colormap'],map);
    Desc(2) = {['colormap: map associated with the indexed image data in "imagedata", returned as a ' num2str(nrow) '-by-3 matrix.']};
else
    Desc(2) = {'colormap: Does not exist, but is included in "imagedata".'};
end
hdf5write(hdf5file,['/figures' '/' figurename '/imagemeta'],figinfo','WriteMode','append');
hdf5write(hdf5file,['/figures' '/' figurename '/DataDescription'],Desc','WriteMode','append');

