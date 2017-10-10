RdeviceScore

## Instalation in Debian 9

See manual: https://cran.r-project.org/bin/linux/debian/

git clone github.com/eReuse/RdeviceScore
install R Language:
apt-get update
apt-get install r-base
sudo apt-get install libcurl4-gnutls-dev

# Install a github package

install.packages("githubinstall")

# Install R


https://cran.r-project.org/src/base/R-3/R-3.3.3.tar.gz

cd R-2.15.3/

./configure –enable-R-shlib

#if problems
./configure –enable-R-shlib –with-readline=no –with-x=no

make clean

make

make install

# ensure installed
ls /usr/local/bin |grep R



# install devtools 

apt-get -y build-dep libcurl4-gnutls-dev
apt-get -y install libcurl4-gnutls-dev