function code=FIRcode(Nbits)
oversample=20;
Bark=Nbits/oversample;
code=Barker(Bark);
%code=col(repmat(code',oversample,1));
code=[col([code(1:end-1)';zeros(oversample-1,Bark-1)]);code(end)];
