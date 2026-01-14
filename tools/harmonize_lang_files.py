import argparse
import os

import yaml

def replace_in_yaml_files(yaml_dir, old_key, new_key):
    """Replace keys in YAML files with the new value."""
    for filename in os.listdir(yaml_dir):
        if not (filename.endswith('.yaml') or filename.endswith('.yml')):
            continue
        filepath = os.path.join(yaml_dir, filename)
        with open(filepath, 'r') as file:
            data = yaml.safe_load(file)
        try:
            if new_key in data:
                print(f"{new_key} already exists in {filename}")
                continue
            if old_key not in data:
                print(f"{old_key} not found in {filename}")
                continue
            data[new_key] = data[old_key]
            data.pop(old_key)
            with open(filepath, 'w') as outfile:
                yaml.dump(data, outfile, default_flow_style=False, allow_unicode=True)
        except yaml.YAMLError as e:
            print(f"Error processing {filename}: {e}")

def replace_in_elm_files(elm_dir, old_string, new_string):
    """Replace strings in Elm files."""
    for root, _, files in os.walk(elm_dir):
        for filename in files:
            if filename.endswith('.elm'):
                filepath = os.path.join(root, filename)
                with open(filepath, 'r') as file:
                    content = file.read()
                content = content.replace(old_string, new_string)
                with open(filepath, 'w') as file:
                    file.write(content)

def main():
    parser = argparse.ArgumentParser(description='Replace keys in YAML files and strings in Elm files.')
    parser.add_argument('old_key', help='The key to replace in YAML files or string to replace in Elm files')
    parser.add_argument('new_value', help='The new value to replace the key with in YAML files or string to replace in Elm files')
    parser.add_argument('--yaml-dir', default='./yaml', help='Directory containing YAML files')
    parser.add_argument('--elm-dir', default='./src', help='Directory containing Elm files')

    args = parser.parse_args()

    replace_in_yaml_files(args.yaml_dir, args.old_key, args.new_value)
    replace_in_elm_files(args.elm_dir, args.old_key, args.new_value)

if __name__ == '__main__':
    main()

