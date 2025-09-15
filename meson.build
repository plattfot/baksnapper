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

test_configuration = {
  'default': {
    'CONFIG': 'root',
    'DAEMON': baksnapperd[0].full_path(),
  },
  'latest': {
    'CONFIG': 'root',
    'DAEMON': baksnapperd[0].full_path(),
    'PRUNE': 'NO',
    'ALL': 'NO',
    'VERBOSE': 'NO',
    'LINK': 'YES',
  },
  'prune': {
    'CONFIG': 'home',
    'DAEMON': baksnapperd[0].full_path(),
    'PRUNE': 'YES',
    'ALL': 'NO',
    'VERBOSE': 'NO',
  },
  'all': {
    'CONFIG': 'root',
    'DAEMON': baksnapperd[0].full_path(),
    'ALL': 'YES',
    'VERBOSE': 'NO',
  },
}
foreach conf: [
  {
    'suffix': '-config',
    'runner-args': [],
    'baksnapper-args': ['--config', 'root'],
    'configfile': false,
  },
  {
    'suffix': '-src-dest',
    'runner-args': ['--src-dest'],
    'baksnapper-args': [],
    'configfile': false,
  },
  {
    'suffix': '-configfile',
    'runner-args': [],
    'baksnapper-args': [],
    'configfile': true,
  },
]
  foreach test_entry: [
    {
      'name': 'default',
      'runner-args': [
        '--sender', '1,2,3',
        '--expected', '3',
      ],
      'baksnapper-args': [
        baksnapper[0].full_path(),
        '--daemon', baksnapperd[0].full_path(),
      ],
      'configfile': 'default',
      'configfile-baksnapper-args': [],
    },
    {
      'name': 'latest',
      'runner-args': [
        '--sender', '1,2,3',
        '--expected', '3',
        '--expected-latest', '3',
      ],
      'baksnapper-args': [
        baksnapper[0].full_path(),
        '--daemon', baksnapperd[0].full_path(),
        '--link',
      ],
      'configfile': 'latest',
      'configfile-baksnapper-args': [],
    },
    {
      'name': 'prune',
      'runner-args': [
        '--sender', '1,2,3,6,7,8',
        '--receiver', '1,2,3,4,5,6',
        '--expected', '1,2,3,6,8:p=6',
      ],
      'baksnapper-args': [
        baksnapper[0].full_path(),
        '--daemon', baksnapperd[0].full_path(),
        '--prune',
      ],
      'configfile': 'prune',
      'configfile-baksnapper-args': ['--config', 'root'],
    },
    {
      'name': 'backup-2',
      'runner-args': [
        '--sender', '1,2,3',
        '--expected', '2',
      ],
      'baksnapper-args': [
        baksnapper[0].full_path(),
        '--daemon', baksnapperd[0].full_path(),
        '--snapshot', '2',
      ],
      'configfile': 'default',
      'configfile-baksnapper-args': [],
    },
    {
      'name': 'delete-1',
      'runner-args': [
        '--sender', '1,2,3',
        '--receiver', '1,2,3',
        '--expected', '2,3',
      ],
      'baksnapper-args': [
        baksnapper[0].full_path(),
        '--daemon', baksnapperd[0].full_path(),
        '--delete', '1',
      ],
      'configfile': 'default',
      'configfile-baksnapper-args': [],
    },
    {
      'name': 'delete-1,3',
      'runner-args': [
        '--sender', '1,2,3',
        '--receiver', '1,2,3',
        '--expected', '2',
      ],
      'baksnapper-args': [
        baksnapper[0].full_path(),
        '--daemon', baksnapperd[0].full_path(),
        '--delete', '1,3',
      ],
      'configfile': 'default',
      'configfile-baksnapper-args': [],
    },
    {
      'name': 'backup-with-right-parent',
      'runner-args': [
        '--sender', '64,65,76,83,91,92,93',
        '--receiver', '64,66:p=64,76:p=66,83:p=76,91:p=83,92:p=83',
        '--expected', '64,66:p=64,76:p=66,83:p=76,91:p=83,92:p=83,93:p=92',
      ],
      'baksnapper-args': [
        baksnapper[0].full_path(),
        '--daemon', baksnapperd[0].full_path(),
      ],
      'configfile': 'default',
      'configfile-baksnapper-args': [],
    },
    # https://github.com/plattfot/baksnapper/issues/27
    {
      'name': 'backup-all-with-right-parent',
      'runner-args': [
        '--sender', '64,65,76,83,91,92,93',
        '--receiver', '64,66:p=64,76:p=66,83:p=76,91:p=83,92:p=83',
        '--expected', '64,65:p=64,66:p=64,76:p=66,83:p=76,91:p=83,92:p=83,93:p=92',
      ],
      'baksnapper-args': [
        baksnapper[0].full_path(),
        '--daemon', baksnapperd[0].full_path(),
        '--all',
      ],
      'configfile': 'all',
      'configfile-baksnapper-args': [],
    }
  ]
    baksnapper_args = []
    test_depends = []
    if conf['configfile']
      test_config = custom_target(
        input: configure_file(
          output: test_entry['name'] + '.bsconf.meson',
          configuration: test_configuration[test_entry['configfile']]
        ),
        output: test_entry['name'] + '.bsconf',
        command: [find_program('tests/bin/config-to-bsconf'), '@INPUT@'],
        capture: true,
      )
      baksnapper_args = [
        '--configfile', test_config.full_path()
      ] + test_entry['configfile-baksnapper-args']
      test_depends = [test_config]
    endif
    test(
      test_entry['name']+conf['suffix'],
      runner,
      env: test_env,
      args: test_entry['runner-args'] + conf['runner-args'] +
            ['--'] +
            test_entry['baksnapper-args'] + conf['baksnapper-args'] + baksnapper_args,
      depends: [
        baksnapper,
        baksnapperd,
      ] + test_depends,
    )
  endforeach
endforeach
