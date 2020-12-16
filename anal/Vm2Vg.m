
function [Vg,Vgv] = Vm2Vg(Vm,B,dVm)

% [Vg,Vgv] = Vm2Vg(Vm,B)
%
% Transforming velocity in local magnetic Vm (magn. east perp, magn. north perp, antiparallel) 
% to local geographic coordinates Vg (east,north,up) given the magnetic
% field in geographical coordinates B = (Bnorth,Beast,Bdown). Vgv is the
% covariance matrix components

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

A = [e;n;b];  % gtm, rotational matrix, Xm = A*Xg --> Xg = C*Xm, with C = A'

C = inv(A);   % mtg  % for a rotational matrix inv(A) = transpose(A), possible to check

Vg = C*Vm;
Vmv = diag(dVm.^2);
Vgv = C*Vmv/C;

end