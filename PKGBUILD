# This is an example PKGBUILD file. Use this as a start to creating your own,
# and remove these comments. For more information, see 'man PKGBUILD'.
# NOTE: Please fill out the license field for your package! If it is unknown,
# then please put 'unknown'.

# Maintainer: Fredrik Salomonsson <plattfot@gmail.com>
pkgname=baksnapper
pkgver=0.6.0
pkgrel=1
epoch=
pkgdesc="Backup script for snapper snapshots."
arch=('any')
url="https://github.com/plattfot/baksnapper"
license=('GPL3')
groups=()
depends=('btrfs-progs' 'snapper')
makedepends=()
checkdepends=()
optdepends=()
provides=()
conflicts=()
replaces=()
backup=()
options=()
install=
changelog=ChangeLog
source=("$pkgname-$pkgver::git+https://github.com/plattfot/baksnapper.git#tag=$pkgver")
noextract=()
md5sums=('SKIP') #generate with 'makepkg -g'

package() {
	cd "$srcdir/$pkgname-$pkgver"
	make install PREFIX="$pkgdir" BSCONF_ROOT=/etc 
}


