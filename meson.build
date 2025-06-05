# SPDX-FileCopyrightText: 2025  Fredrik Salomonsson <plattfot@posteo.net>
#
# SPDX-License-Identifier: GPL-3.0-or-later

project(
  'baksnapper',
  version: run_command('./build-aux/version', check: true).stdout().strip()
)

install_cmd = find_program('install')

baksnapper = custom_target(
  command: [install_cmd, '@INPUT@', '@OUTPUT@'],
  input: 'baksnapper.sh',
  output: 'baksnapper',
  install: true,
  install_dir: 'bin',
  install_mode: 'rwxr-xr-x',
  install_tag: 'runtime',
)

baksnapperd = custom_target(
  command: [install_cmd, '@INPUT@', '@OUTPUT@'],
  input: 'baksnapperd.sh',
  output: 'baksnapperd',
  install: true,
  install_dir: 'bin',
  install_mode: 'rwxr-xr-x',
  install_tag: 'runtime',
)

bsconf_root = get_option('bsconf-root')
if bsconf_root == ''
  bsconf_root = get_option('sysconfdir')
endif

baksnapper_service = configure_file(
  input: 'systemd/baksnapper@.service.in',
  output: 'baksnapper@.service.in',
  configuration: {
    'BSCONF_ROOT': bsconf_root
  },
)

custom_target(
  command: [install_cmd, '@INPUT@', '@OUTPUT@'],
  input: baksnapper_service,
  output: 'baksnapper@.service',
  install: true,
  install_dir: 'lib/systemd/system',
  install_mode: 'rw-r--r--',
  install_tag: 'systemd',
)

custom_target(
  command: [install_cmd, '@INPUT@', '@OUTPUT@'],
  input: 'systemd/baksnapper@.timer',
  output: 'baksnapper@.timer',
  install: true,
  install_dir: 'lib/systemd/system',
  install_mode: 'rw-r--r--',
  install_tag: 'systemd',
)

install_data(
  'systemd/example.bsconf',
  rename: 'root.bsconf',
  install_dir: get_option('sysconfdir')/'baksnapper/example',
  install_tag: 'runtime',
)

test(
  'help',
  baksnapper,
  args: ['--help'],
)

test_env = environment()
test_env.prepend('PATH', meson.current_source_dir() / 'tests/bin')
runner = find_program('./tests/runner.scm')

test(
  'default',
  runner,
  env: test_env,
  args: [
    '--sender', '1,2,3',
    '--expected', '3',
    '--',
    baksnapper[0].full_path(),
    '--config', 'root',
    '--daemon', baksnapperd[0].full_path(),
  ],
  depends: [
    baksnapper,
    baksnapperd,
  ],
)

test(
  'latest',
  runner,
  env: test_env,
  args: [
    '--sender', '1,2,3',
    '--expected', '3',
    '--expected-latest', '3',
    '--',
    baksnapper[0].full_path(),
    '--config', 'root',
    '--daemon', baksnapperd[0].full_path(),
    '--link',
  ],
  depends: [
    baksnapper,
    baksnapperd,
  ],
)


test(
  'backup-2',
  runner,
  env: test_env,
  args: [
    '--sender', '1,2,3',
    '--expected', '2',
    '--',
    baksnapper[0].full_path(),
    '--config', 'root',
    '--daemon', baksnapperd[0].full_path(),
    '--snapshot', '2',
  ],
  depends: [
    baksnapper,
    baksnapperd,
  ],
)

test(
  'prune',
  runner,
  env: test_env,
  args: [
    '--sender', '1,2,3,6,7,8',
    '--receiver', '1,2,3,4,5,6',
    '--expected', '1,2,3,6,8:p=6',
    '--',
    baksnapper[0].full_path(),
    '--config', 'root',
    '--daemon', baksnapperd[0].full_path(),
    '--prune',
  ],
  depends: [
    baksnapper,
    baksnapperd,
  ],
)

test(
  'delete-1',
  runner,
  env: test_env,
  args: [
    '--sender', '1,2,3',
    '--receiver', '1,2,3',
    '--expected', '2,3',
    '--',
    baksnapper[0].full_path(),
    '--config', 'root',
    '--daemon', baksnapperd[0].full_path(),
    '--delete', '1',
  ],
  depends: [
    baksnapper,
    baksnapperd,
  ],
)

test(
  'delete-1,3',
  runner,
  env: test_env,
  args: [
    '--sender', '1,2,3',
    '--receiver', '1,2,3',
    '--expected', '2',
    '--',
    baksnapper[0].full_path(),
    '--config', 'root',
    '--daemon', baksnapperd[0].full_path(),
    '--delete', '1,3',
  ],
  depends: [
    baksnapper,
    baksnapperd,
  ],
)

# https://github.com/plattfot/baksnapper/issues/27
test(
  'backup-all-with-right-parent',
  runner,
  env: test_env,
  args: [
    '--sender', '64,65,76,83,91,92,93',
    '--receiver', '64,66:p=64,76:p=66,83:p=76,91:p=83,92:p=83',
    '--expected', '64,65:p=64,66:p=64,76:p=66,83:p=76,91:p=83,92:p=83,93:p=92',
    '--',
    baksnapper[0].full_path(),
    '--config', 'root',
    '--daemon', baksnapperd[0].full_path(),
    '--all',
  ],
  depends: [
    baksnapper,
    baksnapperd,
  ],
)

test(
  'backup-with-right-parent',
  runner,
  env: test_env,
  args: [
    '--sender', '64,65,76,83,91,92,93',
    '--receiver', '64,66:p=64,76:p=66,83:p=76,91:p=83,92:p=83',
    '--expected', '64,66:p=64,76:p=66,83:p=76,91:p=83,92:p=83,93:p=92',
    '--',
    baksnapper[0].full_path(),
    '--config', 'root',
    '--daemon', baksnapperd[0].full_path(),
  ],
  depends: [
    baksnapper,
    baksnapperd,
  ],
)
