function strds2hdf5_v2(hdffilename,group,dsname,strdata)
% strds2hdf5_v2(hdffilename,group,dsname,strdata)
%
% Writes a dataset of string(s) to a given group in a given HDF5-file.
%
% hdffilename: complete path to and name of HDF5-file to write to.
% group: the group where the string dataset is written, given in the form % group = '/G1/G2/G3 ...'.
% dsname: name of the dataset, 'datasetname'.
% strdata: the string data, in cell form.

if ~strcmp(group(1),'/')
    error('"group" need to start with a /, (/G1/G2/...)')
end

if ~exist(hdffilename)
    fid = H5F.create(hdffilename);
else
    fid = H5F.open(hdffilename,'H5F_ACC_RDWR','H5P_DEFAULT');
end

plist = 'H5P_DEFAULT';
type_id = H5T.copy('H5T_C_S1');

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
   
lchar = max(max(strlength(strdata)));
H5T.set_size(type_id,lchar);
dims = size(strdata);
h5_dims = fliplr(dims);
h5_maxdims = h5_dims;

space_id = H5S.create_simple(2,h5_dims,h5_maxdims);
dcpl = 'H5P_DEFAULT';

dset_id = H5D.create(gid(end),dsname,type_id,space_id,dcpl);

H5D.write(dset_id,'H5ML_DEFAULT','H5S_ALL','H5S_ALL',plist,char(strdata) .'); 
H5S.close(space_id);
H5D.close(dset_id);     
H5T.close(type_id);
H5G.close(gid(end));
H5F.close(fid);

end
