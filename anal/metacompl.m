

function metacompl(OldHdf5file)

global newhdf5file

inform1 = h5info(OldHdf5file,'/Metadata');
metavar = {inform1.Datasets.Name}';
if ~isempty(find(strcmp(metavar,'Experiment Parameters')))
    exprpar = h5read(OldHdf5file,'/Metadata/Experiment Parameters');
else
    warning('No field named "Experiment Parameters"')
    return
end
exprparnames = cellstr(exprpar.name');
exprparvalues = cellstr(exprpar.value');

for ii = 1:length(exprparnames)
   experiment.(char(regexprep(exprparnames(ii),' ','_'))) = exprparvalues{ii};
end

inform2 = h5info(newhdf5file,'/metadata');
metavar = {inform2.Datasets.Name}';
if isempty(find(strcmp(metavar,'experiment')))
    hdf5write(newhdf5file,'/metadata/experiment',experiment,'WriteMode','append');
else
    display('/metadata/experiment already exists in the new hdf5-file.')
    return
end
