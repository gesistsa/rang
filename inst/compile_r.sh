if echo "$1" | grep -Eq '^1' ; then
    DOWNLOAD_DIR="R-1"
    TARFILE="R-$1.tgz"
elif echo "$1" | grep -Eq '^2' ; then
    DOWNLOAD_DIR="R-2"
    TARFILE="R-$1.tar.gz"
elif echo "$1" | grep -Eq '^3' ; then
    DOWNLOAD_DIR="R-3"
    TARFILE="R-$1.tar.gz"
else
    DOWNLOAD_DIR="R-4"
    TARFILE="R-$1.tar.gz"
fi

if [ ! -f "/cache/rsrc/$TARFILE" ]; then
    wget "http://cran.r-project.org/src/base/$DOWNLOAD_DIR/$TARFILE"
else
    cp /cache/rsrc/$TARFILE /
fi

tar -zxf $TARFILE

cd "R-$1"
./configure --without-x
make
make install

cd /

Rscript rang.R
