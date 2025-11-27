# SPDX-FileCopyrightText: 2025  Fredrik Salomonsson <plattfot@posteo.net>
#
# SPDX-License-Identifier: GPL-3.0-or-later

project(
  'baksnapper',
  meson_version: '>=1.4',
  version: run_command('./build-aux/version', check: true).stdout().strip()
)

install_cmd = find_program('install')

config_data = configuration_data({
    'VERSION': meson.project_version(),
})

baksnapper = configure_file(
  input: 'baksnapper.sh',
  output: 'baksnapper',
  configuration: config_data,
  install: true,
  install_dir: 'bin',
  install_mode: 'rwxr-xr-x',
  install_tag: 'runtime',
)

baksnapperd = configure_file(
  input: 'baksnapperd.sh',
  output: 'baksnapperd',
  configuration: config_data,
  install: true,
  install_dir: 'bin',
  install_mode: 'rwxr-xr-x',
  install_tag: 'runtime',
)

baksnapperd_denotebak = configure_file(
  input: 'baksnapperd-denotebak.sh',
  output: 'baksnapperd-denotebak',
  configuration: config_data,
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
test_env.set('MOCK_SSH_CONNECTION', '0')
test_env.set('MOCK_SSH_KEY', 'foo.key=foo.example.com:bar.key=bar.example.com')
test_env.set('MOCK_SSH_BAKSNAPPERD', baksnapperd.full_path())

denotebak_test_env = test_env
denotebak_test_env.set('MOCK_SSH_BAKSNAPPERD', baksnapperd_denotebak.full_path())

runner = find_program('./tests/runner.scm')

test_configuration = {
  'default': {
    'DAEMON': baksnapperd.full_path(),
  },
  'latest': {
    'DAEMON': baksnapperd.full_path(),
    'PRUNE': 'NO',
    'ALL': 'NO',
    'VERBOSE': 'NO',
    'LINK': 'YES',
  },
  'prune': {
    'DAEMON': baksnapperd.full_path(),
    'PRUNE': 'YES',
    'ALL': 'NO',
    'VERBOSE': 'NO',
  },
  'all': {
    'DAEMON': baksnapperd.full_path(),
    'ALL': 'YES',
    'VERBOSE': 'NO',
  },
  'denotebak-default': {
    'DAEMON': baksnapperd_denotebak.full_path(),
  },
  'denotebak-latest': {
    'DAEMON': baksnapperd_denotebak.full_path(),
    'PRUNE': 'NO',
    'ALL': 'NO',
    'VERBOSE': 'NO',
    'LINK': 'YES',
  },
  'denotebak-prune': {
    'DAEMON': baksnapperd_denotebak.full_path(),
    'PRUNE': 'YES',
    'ALL': 'NO',
    'VERBOSE': 'NO',
  },
  'denotebak-all': {
    'DAEMON': baksnapperd_denotebak.full_path(),
    'ALL': 'YES',
    'VERBOSE': 'NO',
  },
}
foreach test_entry: [
  {
    'name': 'default',
    'runner-args': [
      '--sender', '1,2,3',
      '--expected', '3',
    ],
    'baksnapper-args': [
      baksnapper,
      '--daemon', baksnapperd,
    ],
    'configfile': 'default',
    'configfile-baksnapper-args': [],
    'suite': ['backup'],
    'env': test_env,
  },
  {
    'name': 'latest',
    'runner-args': [
      '--sender', '1,2,3',
      '--expected', '3',
      '--expected-latest', '3',
    ],
    'baksnapper-args': [
      baksnapper,
      '--daemon', baksnapperd,
      '--link',
    ],
    'configfile': 'latest',
    'configfile-baksnapper-args': [],
    'suite': ['latest'],
    'env': test_env,
  },
  {
    'name': 'prune',
    'runner-args': [
      '--sender', '1,2,3,6,7,8',
      '--receiver', '1,2,3,4,5,6',
      '--expected', '1,2,3,6,8:p=6',
    ],
    'baksnapper-args': [
      baksnapper,
      '--daemon', baksnapperd,
      '--prune',
    ],
    'configfile': 'prune',
    'configfile-baksnapper-args': [],
    'suite': ['prune'],
    'env': test_env,
  },
  {
    'name': 'backup-2',
    'runner-args': [
      '--sender', '1,2,3',
      '--expected', '2',
    ],
    'baksnapper-args': [
      baksnapper,
      '--daemon', baksnapperd,
      '--snapshot', '2',
    ],
    'configfile': 'default',
    'configfile-baksnapper-args': [],
    'suite': ['backup'],
    'env': test_env,
  },
  {
    'name': 'delete-1',
    'runner-args': [
      '--sender', '1,2,3',
      '--receiver', '1,2,3',
      '--expected', '2,3',
    ],
    'baksnapper-args': [
      baksnapper,
      '--daemon', baksnapperd,
      '--delete', '1',
    ],
    'configfile': 'default',
    'configfile-baksnapper-args': [],
    'suite': ['delete'],
    'env': test_env,
  },
  {
    'name': 'delete-1,3',
    'runner-args': [
      '--sender', '1,2,3',
      '--receiver', '1,2,3',
      '--expected', '2',
    ],
    'baksnapper-args': [
      baksnapper,
      '--daemon', baksnapperd,
      '--delete', '1,3',
    ],
    'configfile': 'default',
    'configfile-baksnapper-args': [],
    'suite': ['delete'],
    'env': test_env,
  },
  {
    'name': 'backup-with-right-parent',
    'runner-args': [
      '--sender', '64,65,76,83,91,92,93',
      '--receiver', '64,66:p=64,76:p=66,83:p=76,91:p=83,92:p=83',
      '--expected', '64,66:p=64,76:p=66,83:p=76,91:p=83,92:p=83,93:p=92',
    ],
    'baksnapper-args': [
      baksnapper,
      '--daemon', baksnapperd,
    ],
    'configfile': 'default',
    'configfile-baksnapper-args': [],
    'suite': ['backup'],
    'env': test_env,
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
      baksnapper,
      '--daemon', baksnapperd,
      '--all',
    ],
    'configfile': 'all',
    'configfile-baksnapper-args': [],
    'suite': ['backup'],
    'env': test_env,
  },
  # DeNotebak
  {
    'name': 'denotebak-default',
    'runner-args': [
      '--sender', ','.join([
        '20251023T130000:t=dailies',
        '20251025T120000',
        '20251025T160000:t=hourly']),
      '--expected', ','.join([
        '20251025T160000']),
      '--type', 'denotebak',
    ],
    'baksnapper-args': [
      baksnapper,
      '--daemon', baksnapperd_denotebak,
    ],
    'configfile': 'denotebak-default',
    'configfile-baksnapper-args': [],
    'suite': ['denotebak', 'backup'],
    'env': denotebak_test_env,
  },
  {
    'name': 'denotebak-latest',
    'runner-args': [
      '--sender', ','.join([
        '20251023T130000:t=dailies',
        '20251025T120000',
        '20251025T160000:t=hourly']),
      '--expected', ','.join([
        '20251025T160000']),
      '--expected-latest', '20251025T160000',
      '--type', 'denotebak',
    ],
    'baksnapper-args': [
      baksnapper,
      '--daemon', baksnapperd_denotebak,
      '--link',
    ],
    'configfile': 'denotebak-latest',
    'configfile-baksnapper-args': [],
    'suite': ['denotebak', 'latest'],
    'env': denotebak_test_env,
  },
  {
    'name': 'denotebak-prune',
    'runner-args': [
      '--sender', ','.join([
        '20251023T130000:t=dailies',
        '20251025T120000',
        '20251025T130000:t=dailies',
        '20251025T140000:t=hourly',
        '20251025T150000:t=hourly',
        '20251025T160000:t=hourly']),
      '--receiver', ','.join([
        '20251023T130000',
        '20251023T180000',
        '20251024T090000',
        '20251024T130000',
        '20251024T180000',
        '20251025T120000',
        '20251025T130000']),
      '--expected', ','.join([
        '20251023T130000',
        '20251025T120000',
        '20251025T130000',
        '20251025T160000:p=20251025T130000']),
      '--type', 'denotebak',
    ],
    'baksnapper-args': [
      baksnapper,
      '--daemon', baksnapperd_denotebak,
      '--prune',
    ],
    'configfile': 'denotebak-prune',
    'configfile-baksnapper-args': [],
    'suite': ['denotebak', 'prune'],
    'env': denotebak_test_env,
  },
  {
    'name': 'denotebak-backup-20251025T150000',
    'runner-args': [
      '--sender', ','.join([
        '20251025T140000:t=hourly',
        '20251025T150000:t=hourly',
        '20251025T160000:t=hourly']),
      '--expected', ','.join([
        '20251025T150000']),
      '--type', 'denotebak',
    ],
    'baksnapper-args': [
      baksnapper,
      '--daemon', baksnapperd_denotebak,
      '--snapshot', '20251025T150000',
    ],
    'configfile': 'denotebak-default',
    'configfile-baksnapper-args': [],
    'suite': ['denotebak', 'backup'],
    'env': denotebak_test_env,
  },
  {
    'name': 'denotebak-delete-20251025T140000',
    'runner-args': [
      '--sender', ','.join([
        '20251025T140000:t=hourly',
        '20251025T150000:t=hourly',
        '20251025T160000:t=hourly']),
      '--receiver', ','.join([
        '20251025T140000',
        '20251025T150000',
        '20251025T160000']),
      '--expected', ','.join([
        '20251025T150000',
        '20251025T160000']),
      '--type', 'denotebak',
    ],
    'baksnapper-args': [
      baksnapper,
      '--daemon', baksnapperd_denotebak,
      '--delete', '20251025T140000',
    ],
    'configfile': 'denotebak-default',
    'configfile-baksnapper-args': [],
    'suite': ['denotebak', 'delete'],
    'env': denotebak_test_env,
  },
  {
    'name': 'denotebak-delete-20251025T140000,20251025T160000',
    'runner-args': [
      '--sender', ','.join([
        '20251025T140000:t=hourly',
        '20251025T150000:t=hourly',
        '20251025T160000:t=hourly']),
      '--receiver', ','.join([
        '20251025T140000:t=hourly',
        '20251025T150000',
        '20251025T160000']),
      '--expected', ','.join([
        '20251025T150000']),
      '--type', 'denotebak',
    ],
    'baksnapper-args': [
      baksnapper,
      '--daemon', baksnapperd_denotebak,
      '--delete', '20251025T140000,20251025T160000',
    ],
    'configfile': 'denotebak-default',
    'configfile-baksnapper-args': [],
    'suite': ['denotebak', 'delete'],
    'env': denotebak_test_env,
  },
  {
    'name': 'denotebak-backup-with-right-parent',
    'runner-args': [
      '--sender', ','.join([
        '20251023T130000:t=dailies',
        '20251023T180000:t=dailies',
        '20251024T090000:t=dailies',
        '20251024T130000:t=dailies',
        '20251024T180000:t=dailies',
        '20251025T120000',
        '20251025T130000:t=dailies',
        '20251025T140000:t=hourly',
        '20251025T150000:t=hourly',
        '20251025T160000:t=hourly']),
      '--receiver', ','.join([
        '20251023T130000',
        '20251023T180000:p=20251023T130000',
        '20251024T090000:p=20251023T180000',
        '20251024T130000:p=20251024T090000',
        '20251024T180000:p=20251024T130000',
        '20251025T120000:p=20251024T180000',
        '20251025T130000:p=20251025T120000']),
      '--expected', ','.join([
        '20251023T130000',
        '20251023T180000:p=20251023T130000',
        '20251024T090000:p=20251023T180000',
        '20251024T130000:p=20251024T090000',
        '20251024T180000:p=20251024T130000',
        '20251025T120000:p=20251024T180000',
        '20251025T130000:p=20251025T120000',
        '20251025T160000:p=20251025T130000']),
      '--type', 'denotebak',
    ],
    'baksnapper-args': [
      baksnapper,
      '--daemon', baksnapperd_denotebak,
    ],
    'configfile': 'denotebak-default',
    'configfile-baksnapper-args': [],
    'suite': ['denotebak', 'backup'],
    'env': denotebak_test_env,
  },
  {
    'name': 'denotebak-backup-all-with-right-parent',
    'runner-args': [
      '--sender', ','.join([
        '20251023T130000:t=dailies',
        '20251023T180000:t=dailies',
        '20251024T090000:t=dailies',
        '20251024T130000:t=dailies',
        '20251024T180000:t=dailies',
        '20251025T120000',
        '20251025T130000:t=dailies',
        '20251025T140000:t=hourly',
        '20251025T150000:t=hourly',
        '20251025T160000:t=hourly']),
      '--receiver', ','.join([
        '20251023T130000',
        '20251023T180000:p=20251023T130000',
        '20251024T090000:p=20251023T180000',
        '20251024T130000:p=20251024T090000',
        '20251024T180000:p=20251024T130000',
        '20251025T120000:p=20251024T180000',
        '20251025T130000:p=20251025T120000']),
      '--expected', ','.join([
        '20251023T130000',
        '20251023T180000:p=20251023T130000',
        '20251024T090000:p=20251023T180000',
        '20251024T130000:p=20251024T090000',
        '20251024T180000:p=20251024T130000',
        '20251025T120000:p=20251024T180000',
        '20251025T130000:p=20251025T120000',
        '20251025T140000:p=20251025T130000',
        '20251025T150000:p=20251025T140000',
        '20251025T160000:p=20251025T150000']),
      '--type', 'denotebak',
    ],
    'baksnapper-args': [
      baksnapper,
      '--daemon', baksnapperd_denotebak,
      '--all',
    ],
    'configfile': 'denotebak-all',
    'configfile-baksnapper-args': [],
    'suite': ['denotebak', 'backup'],
    'env': denotebak_test_env,
  },
]
  foreach conf: [
    {
      'suffix': '-config',
      'runner-args': [],
      'baksnapper-args': ['--config', 'root'],
      'configfile': false,
      'suite': ['config'],
    },
    {
      'suffix': '-src-dest',
      'runner-args': ['--src-dest'],
      'baksnapper-args': [],
      'configfile': false,
      'suite': ['src-dest'],
    },
    {
      'suffix': '-configfile',
      'runner-args': [],
      'baksnapper-args': [],
      'configfile': true,
      'suite': ['configfile'],
    },
    {
      'suffix': '-configfile-path',
      'runner-args': ['--path-config'],
      'baksnapper-args': [],
      'configfile': true,
      'suite': ['configfile', 'config'],
    },
    {
      'suffix': '-configfile-src-dest',
      'runner-args': ['--src-dest'],
      'baksnapper-args': [],
      'configfile': true,
      'suite': ['configfile', 'src-dest'],
    },
    {
      'suffix': '-configfile-src',
      'runner-args': ['--src-dest', '--src-config'],
      'baksnapper-args': [],
      'configfile': true,
      'suite': ['configfile', 'src-dest'],
    },
    {
      'suffix': '-configfile-dest',
      'runner-args': ['--src-dest', '--dest-config'],
      'baksnapper-args': [],
      'configfile': true,
      'suite': ['configfile', 'src-dest'],
    },
    {
      'suffix': '-configfile-both',
      'runner-args': ['--src-dest', '--src-config', '--dest-config'],
      'baksnapper-args': [],
      'configfile': true,
      'suite': ['configfile', 'src-dest'],
    },
  ]
    foreach ssh: [
      {
        'suffix': '',
        'args': [],
        'suite': [],
      },
      {
        'suffix': '-ssh-src',
        'args': ['--src-ssh', 'foo.example.com'],
      },
      {
        'suffix': '-ssh-dest',
        'args': ['--dest-ssh', 'bar.example.com'],
      },
      {
        'suffix': '-ssh-both',
        'args': ['--src-ssh', 'foo.example.com',
                 '--dest-ssh', 'bar.example.com'],
      },
    ]
      foreach ssh_key: [
        {
          'suffix': '',
          'args': [],
        },
        {
          'suffix': '-key-src',
          'args': ['--src-private-key', 'foo.key'],
        },
        {
          'suffix': '-key-dest',
          'args': ['--dest-private-key', 'bar.key'],
        },
        {
          'suffix': '-key-both',
          'args': ['--src-private-key', 'foo.key',
                   '--dest-private-key', 'bar.key'],
        },
      ]
        runner_args = []
        baksnapper_args = []
        test_depends = []
        if conf['configfile']
          test_config = custom_target(
            input: configure_file(
              output: test_entry['name'] + ssh['suffix'] + ssh_key['suffix'] +
                      conf['suffix'] + '.bsconf.meson',
              configuration: test_configuration[test_entry['configfile']]
            ),
            output: test_entry['name'] + ssh['suffix'] + ssh_key['suffix'] +
                    conf['suffix'] + '.bsconf',
            command: [find_program('tests/bin/config-to-bsconf'), '@INPUT@'],
            capture: true,
          )
          runner_args += ['--configfile', test_config.full_path()]
          baksnapper_args += test_entry['configfile-baksnapper-args']
          test_depends += [test_config]
        endif

        # The DeNotebak backend does not support --config
        if 'denotebak' in test_entry['suite'] and 'src-dest' not in conf['suite']
          continue
        endif

        test(
          test_entry['name']+conf['suffix']+ssh['suffix']+ssh_key['suffix'],
          runner,
          env: test_entry['env'],
          args: test_entry['runner-args'] + conf['runner-args'] +
                ssh['args'] + runner_args +
                ['--'] +
                test_entry['baksnapper-args'] + conf['baksnapper-args'] +
                ssh_key['args'] + baksnapper_args,
          depends: test_depends,
          suite: conf['suite'] + test_entry['suite'],
        )
      endforeach
    endforeach
  endforeach
endforeach
test(
  'not-enough-inputs',
  baksnapper,
  env: test_env,
  should_fail: true,
)

test(
  'not-enough-inputs-src-dest',
  baksnapper,
  args: ['.'],
  env: test_env,
  should_fail: true,
)
