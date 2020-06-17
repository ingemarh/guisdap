
function [Vg,dVg] = Vm2Vg(Vm,B,dVm)

% Vg = Vm2Vg(Vm,B)
%
% Transforming velocity in local magnetic Vm (magn. east perp, magn. north perp, antiparallel) 
% to local geographic coordinates Vg (east, north, up) given the magnetic
% field in geographical coordinates B = (Be,Bn,Bu).

if nargin<2, error('Error: Magnetic field vector is needed.'), end

B = row(B);                     % n, e, d
Vm = col(Vm);
dVm = col(dVm);

Ba = [-B(2) -B(1) B(3)];        % e, n, u (antiB, with 'up' defined as positive)
Ben = [-Ba(1:2) 0];
b = Ba/norm(B);                          % antipar (z)
e = cross(b,Ben)/norm(cross(b,Ben));     % eperp (x)
if e(1)<0
    e = -e;
end
n = cross(b,e);                          % nperp (y)   

A = [e;n;b];    % rotational matrix, Xm = A*Xg --> Xg = C*Xm, with C = A'

C = A';

Vg = C*Vm;
dVg = sqrt(C.^2*(Vg.*dVm./Vm).^2);



