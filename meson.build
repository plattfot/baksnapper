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
