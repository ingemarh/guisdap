#!/bin/sh -e
if [ "$pkgdir" ]
then
 destdir=${pkgdir}/opt/guisdap
 export PKG=dum
else
 destdir=$PWD
fi
rm -rf build
mkdir build
cd build
echo "Building GUISDAP library project, destination directory is: $destdir"
cmake .. && make
install_cmd="install"
os_type=$(uname)
if [ "$os_type" = "Darwin" ]; then
    install_cmd="ginstall"
fi

for d in lib irbem share/doc share/iri
do
  #echo "installing $d"
  find $d -type f -exec ${install_cmd} -Dvm 644 {} ${destdir}/{} \;
done
rm -rf ${destdir}/share/TS07
mv share/TS07 ${destdir}/share/
cd ..
rm -r build
