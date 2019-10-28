

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

chunklim = 100;
%keyboard
if ~isempty(X)
    Xsize = size(X); 
    nrow = Xsize(1);
    ncol = Xsize(2);
    if ge(nrow,chunklim) && ge(ncol,chunklim), csize = [chunklim chunklim];
    elseif ge(nrow,chunklim), csize = [chunklim ncol];
    elseif ge(ncol,chunklim), csize = [nrow chunklim];
    else, csize = [nrow ncol]; 
    end   
    if length(Xsize)==3
        ndepth = Xsize(3);
        csize = [csize ndepth];
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
end
hdf5write(hdf5file,['/figures' '/' figurename '/imagemeta'],figinfo','WriteMode','append');



% hdf5write(hdf5file,['/figures' '/' figurename '/imagedata'],X,'WriteMode','append');
% hdf5write(hdf5file,['/figures' '/' figurename '/colormap'],map,'WriteMode','append');
% hdf5write(hdf5file,['/figures' '/' figurename '/imagemeta'],figinfo','WriteMode','append');
% 



