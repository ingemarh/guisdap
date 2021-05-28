function strds2hdf5(hdffilename,group,dsname,data)
% strds2hdf5(hdffilename,group,dsname,data)
%
% Writes a dataset of string(s) to a given group in a given HDF5-file.
%
% hdffilename: complete path to and name of HDF5-file to write to.
% group: the group where the string dataset is written, given in the form % group = '/G1/G2/G3 ...'.
% dsname: name of the dataset, 'datasetname'.
% data: the string data, in cell form.

if ~strcmp(group(1),'/')
    error('"group" need to start with a /, (/G1/G2/...)')
end

plist='H5P_DEFAULT';
if ~ischar(hdffilename)
    fid=hdffilename;
elseif ~exist(hdffilename)
    fid = H5F.create(hdffilename);
else
    fid = H5F.open(hdffilename,'H5F_ACC_RDWR',plist);
end

ch=0;
chunklim=1024;
if isa(data,'single')
	type_id = H5T.copy('H5T_NATIVE_FLOAT');
elseif isa(data,'double')
	type_id = H5T.copy('H5T_NATIVE_DOUBLE');
elseif isa(data,'uint8')
	type_id = H5T.copy('H5T_NATIVE_UINT8');
else %char
	ch=1;
	type_id = H5T.copy('H5T_C_S1');
end

gg = [];
try gid = H5G.open(fid,group);
catch
    groups = regexp(group,'/','split')';
    groups = groups(2:end);
    for ii = 1:length(groups)
        gg = [gg '/' char(groups(ii))];
        try   gid = H5G.open(fid,gg);
        catch gid = H5G.create(fid,gg,plist,plist,plist); 
        end
    end
end

if ch
 if length(data) == 1
    H5T.set_size(type_id,'H5T_VARIABLE')
    H5T.set_cset(type_id,H5ML.get_constant_value('H5T_CSET_UTF8'));
 else
    lchar = max(max(strlength(data)));
    H5T.set_size(type_id,lchar);
 end
end

dims = size(data);
h5_dims = fliplr(dims);
h5_maxdims = h5_dims;
space_id = H5S.create_simple(length(dims),h5_dims,h5_maxdims);
csize=h5_dims;
csize(find(csize>chunklim))=chunklim;
if prod(dims) > 32
 dcpl = H5P.create('H5P_DATASET_CREATE'); H5P.set_deflate(dcpl,9); H5P.set_chunk(dcpl,h5_dims);
else
 dcpl='H5P_DEFAULT';
end

dset_id = H5D.create(gid(end),dsname,type_id,space_id,dcpl);
if ch
 H5D.write(dset_id,'H5ML_DEFAULT','H5S_ALL','H5S_ALL',plist,char(data) .');
else
 H5D.write(dset_id,'H5ML_DEFAULT','H5S_ALL','H5S_ALL',plist,data);
end
H5P.close(dcpl);
H5S.close(space_id);
H5D.close(dset_id);
H5T.close(type_id);
H5G.close(gid(end));
if ischar(hdffilename)
 H5F.close(fid);
end
