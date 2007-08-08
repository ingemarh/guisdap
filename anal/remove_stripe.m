function fix=zerolagfix(d_0lag,d_true0,norm)
%fix=d_0lag-conv2(d_true0,ones(length(d_true0)-length(d_0lag)+1,1),'valid')/(norm)^2;
nfir=length(d_true0)-length(d_0lag)+1;
stripe=conv(d_true0,ones(nfir,1)/norm^2);
fix=d_0lag-stripe(nfir:length(d_true0));
