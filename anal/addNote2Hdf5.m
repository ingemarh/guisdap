function addNote2Hdf5(notefile,hdf5file,N)
%addNote2Hdf5(notefile,hdf5file) adds any text (comments, notes etc) in 'notefile' 
%to 'hdf5file'in /metadata/commentN, where N is a number (left out if N = 1) 

if nargin<3
    N = 1;
end
if nargin<2 
    error('Input file(s) is(are) missing!')
end

fid = fopen(notefile);
notes = textscan(fid,'%s','delimiter','');
notes_lines = notes{1};
notes_str = [];
for nc = 1:length(notes_lines)
     if nc == length(notes_lines)
         notes_str = [notes_str char(notes_lines(nc))];
     else
         notes_str = [notes_str char(notes_lines(nc)) newline];
     end
end

notes_str = erase(notes_str,["<BR>","<B>","</B>"," <a","</a>","href=",">"]);
notes_str = regexprep(notes_str,'"','');

if N == 1
    strds2hdf5(hdf5file,'/metadata','comments',{notes_str})
else
    strds2hdf5(hdf5file,'/metadata',['comments' num2str(N)],{notes_str})
end