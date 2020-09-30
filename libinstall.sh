#!/bin/sh -e
if [ "$pkgdir" ]
then
 destdir=${pkgdir}/opt/$pkgname
else
 destdir=$PWD
fi
rm -rf build
mkdir build
cd build
echo $destdir
cmake .. && make
for d in lib irbem share/doc share/iri
do
  #echo "installing $d"
  find $d -type f -exec install -Dvm 644 {} ${destdir}/{} \;
done
rm -rf ${destdir}/share/TS07
mv share/TS07 ${destdir}/share/
cd ..
rm -r build
