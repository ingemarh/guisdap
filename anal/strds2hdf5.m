function strds2hdf5(gid,dsname,strdata)

type_id = H5T.copy('H5T_C_S1');

lchar = max(max(strlength(strdata)));
%H5T.set_size(type_id,'H5T_VARIABLE')
H5T.set_size(type_id,lchar);
%H5T.set_strpad(type_id,'H5T_STR_NULLTERM');
dims = size(strdata);
h5_dims = fliplr(dims);
h5_maxdims = h5_dims;

space_id = H5S.create_simple(2,h5_dims,h5_maxdims);
dcpl = 'H5P_DEFAULT';

dset_id = H5D.create(gid,dsname,type_id,space_id,dcpl);
plist = 'H5P_DEFAULT';

H5D.write(dset_id,'H5ML_DEFAULT','H5S_ALL','H5S_ALL',plist,char(strdata) .'); 

H5S.close(space_id);
H5D.close(dset_id);     
H5T.close(type_id);

end