# Maintainer: Fredrik Salomonsson <plattfot@gmail.com>
pkgname=baksnapper
pkgver=0.8.0
pkgrel=1
epoch=
pkgdesc="Backup script for snapper snapshots."
arch=('any')
url="https://github.com/plattfot/baksnapper"
license=('GPL3')
groups=()
depends=('btrfs-progs' 'snapper' 'util-linux')
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
md5sums=('SKIP')
package() {
	cd "$srcdir/$pkgname-$pkgver"
	make install PREFIX="$pkgdir" BSCONF_ROOT=/etc 
}




