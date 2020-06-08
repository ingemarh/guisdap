

function metacompl(OldHdf5file,EISCAThdf5file)
% metacompl(OldHdf5file,EISCAThdf5file)
% If experiment metadata is missing in EISCAThdf5file, extract experiment 
% metadata from older HDF5-file (OldHdf5file) and insert in the new 
% HDF5-file (EISCAThdf5file)

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
   experiment.(char(regexprep(exprparnames(ii),' ','_'))) = {exprparvalues{ii}};
end

inform2 = h5info(EISCAThdf5file,'/metadata');
metagroups = {inform2.Datasets.Name}';
if isempty(find(strcmp(metagroups,'/metadata/software/gfd'))) && isempty(find(strcmp(metagroups,'/metadata/software/experiment')))
    fields = fieldnames(experiment);
    for ii = 1:length(fields)
        strds2hdf5(EISCAThdf5file,'/metadata/software/experiment',char(fields(ii)),experiment.(char(fields(ii))))
    end
else
    display('/metadata/software/gfd or /metadata/software/experiment already exists in the new HDF5 file.')
    return
end
