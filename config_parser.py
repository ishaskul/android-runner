import os.path as op
import sys
import errno
from imp import find_module
import json


class ConfigError(Exception):
    """Raised when a config error occurred"""
    pass


def load_json(path):
    try:
        with open(path, 'r') as f:
            try:
                return json.loads(f.read())
            except ValueError:
                print("'%s' is not valid JSON" % path)
                raise
    except IOError as e:
        if e.errno == errno.ENOENT:
            print("'%s' not found" % path)
        raise


def get_value(obj, key, mandatory=False, default=None):
    try:
        return obj[key]
    except KeyError:
        if mandatory:
            raise ConfigError("Key '%s' does not exist" % key)
        else:
            return default


def can_be_imported(script):
    try:
        find_module(op.splitext(script)[0])
        return True
    except ImportError:
        raise ConfigError("'%s' cannot be imported" % script)


def find_device_ids(devices):
    try:
        ids = load_json('devices.json')
        for device in filter(lambda dev: not ids.get(dev, None), devices):
            raise ConfigError("Device '%s' is not found in devices.json" % device)
        return [x for x in [ids.get(d, None) for d in devices] if x is not None]
    except (ValueError, IOError):
        sys.exit(1)


def test_apks(apks):
    for f in filter(lambda x: not op.isfile(x), apks):
        raise ConfigError("File '%s' not found" % f)


class ConfigParser:
    def __init__(self, config_file):
        self.config = None
        self.errors = []
        # Config file keys
        self.mandatory_keys = ['name', 'devices', 'type', 'replications', 'measurements', 'scripts']
        # Default values
        self.defaults = {
            'interface': 'adb',
            'paths': [],
            'basedir': op.abspath(op.dirname(config_file))
        }
        try:
            self.config = load_json(config_file)
        except (ValueError, IOError):
            sys.exit(1)

    def append_exceptions(self, func, *args, **kwargs):
        try:
            return func(*args, **kwargs)
        except ConfigError as e:
            self.errors.append(e.message)

    def parse(self):
        parsed_config = {}
        e = self.append_exceptions
        for k in self.mandatory_keys:
            parsed_config[k] = e(get_value, self.config, k, mandatory=True)

        for k, v in self.defaults.items():
            parsed_config[k] = e(get_value, self.config, k, default=self.defaults[k])

        if parsed_config['type'] == 'web':
            parsed_config['browsers'] = e(get_value, self.config, 'browsers', mandatory=True)

        sys.path.append(parsed_config['basedir'])
        for _, s in parsed_config['scripts'].items():
            e(can_be_imported, s)

        parsed_config['devices'] = e(find_device_ids, parsed_config['devices'])

        if self.errors:
            raise ConfigError(self.errors)
        return parsed_config
