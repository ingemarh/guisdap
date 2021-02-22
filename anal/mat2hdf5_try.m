function mat2hdf5_try(matpath,datapath,addfigs,addnotes) 

try
    mat2hdf5(matpath,datapath,addfigs,addnotes); 
catch e
    display(['Crashed when running mat2hdf5. The error message: ' e.message ' (error identifier: ' e.identifier ')'])
    quit
end
    
    