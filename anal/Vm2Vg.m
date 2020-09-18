
function [Vg,dVg] = Vm2Vg(Vm,B,dVm)

% Vg = Vm2Vg(Vm,B)
%
% Transforming velocity in local magnetic Vm (magn. east perp, magn. north perp, antiparallel) 
% to local geographic coordinates Vg (east,north,up) given the magnetic
% field in geographical coordinates B = (Bnorth,Beast,Bdown).

if nargin<2, error('Error: Magnetic field vector is needed.'), end

B = row(B);                     % n, e, d
Vm = col(Vm);                   % eperp, nperp, antiB
dVm = col(dVm);

Ba = [-B(2) -B(1) B(3)];        % e, n, u (with 'up' defined as positive)
Ben = [-Ba(1:2) 0];
b = Ba/norm(B);                          % antiB (z)
e = cross(b,Ben)/norm(cross(b,Ben));     % eperp (x)
if e(1)<0
    e = -e;
end
n = cross(b,e);                          % nperp (y)   

A = [e;n;b];    % rotational matrix, Xm = A*Xg --> Xg = C*Xm, with C = A'

C = A';

Vg = C*Vm;

T=(C'*(C./(dVm.^2*ones(1,3))));

if isposdef(T)
    T = T^(-1);
    dVg = sqrt(diag(T));
else
    warning('Covariance matrix not positive definite')
    % trying direct route, but errors handled in a clumsy way
    [~,dVg]=lscov(C,Vm,1../dVm.^2);
    if size(C,1) == 3
        dVg = abs(C)\dVm;
    end
end


function ret=isposdef(M)
ret=true;
for i=1:length(M)
  if det(M(1:i,1:i))<=0
    ret=false;
    break
  end
end
return

