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
            if new_key and new_key in data:
                print(f"{new_key} already exists in {filename}")
                continue
            if old_key not in data:
                print(f"{old_key} not found in {filename}")
                continue
            if new_key:
                data[new_key] = data[old_key]
            data.pop(old_key)
            with open(filepath, 'w') as outfile:
                yaml.dump(data, outfile, default_flow_style=False, allow_unicode=True, sort_keys=False)
        except yaml.YAMLError as e:
            print(f"Error processing {filename}: {e}")

def capitalize_first_letter(input_string):
    if not input_string:
        return input_string
    return input_string[0].upper() + input_string[1:]

def capitalize_first_letter_lower_rest(input_string):
    if not input_string:
        return input_string
    return input_string[0].upper() + input_string[1:].lower()

def enquote(string):
    return f'"{string}"'

def replace_in_elm_files(elm_dir, old_string, new_string):
    """Replace strings in Elm files."""
    if not new_string:
        return
    changed = False
    for root, _, files in os.walk(elm_dir):
        for filename in files:
            if filename.endswith('.elm'):
                filepath = os.path.join(root, filename)
                with open(filepath, 'r') as file:
                    orig_content = file.read()
                content = orig_content
                content = content.replace(
                    enquote(capitalize_first_letter(old_string)), 
                    enquote(capitalize_first_letter(new_string)))
                content = content.replace(
                    enquote(capitalize_first_letter_lower_rest(old_string)), 
                    enquote(capitalize_first_letter(new_string)))
                content = content.replace(enquote(old_string), enquote(new_string))
                changed = changed or content != orig_content
                with open(filepath, 'w') as file:
                    file.write(content)
    if not changed and new_string:
        print(f"{old_string} not found")
    return changed


def main():
    parser = argparse.ArgumentParser(description='Replace keys in YAML files and strings in Elm files.')
    parser.add_argument('--replacement-file', help='File containing replacement keys as YAML file')
    parser.add_argument('--yaml-dir', default='./yaml', help='Directory containing YAML files')
    parser.add_argument('--elm-dir', default='./src', help='Directory containing Elm files')

    args = parser.parse_args()

    with open(args.replacement_file, 'r') as file:
        kv = yaml.safe_load(file)
    for old_key, new_key in kv.items():
        changed = replace_in_elm_files(args.elm_dir, old_key, new_key)
        if not changed:
            new_key = ""
        replace_in_yaml_files(args.yaml_dir, old_key, new_key)

if __name__ == '__main__':
    main()

