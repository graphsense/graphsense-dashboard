import sys

import yaml

def merge_yaml(file1, file2, output_file):
    with open(file1, 'r') as f1, open(file2, 'r') as f2:
        de = yaml.safe_load(f1)
        en = yaml.safe_load(f2)

    for k in de:
        if not en.get(k, None): 
            en[k] = de[k]

    with open(output_file, 'w') as out:
        yaml.dump(en, out, default_flow_style=False, allow_unicode=True)

if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("Usage: python merge_yaml.py <de file> <en file> <output_file>")
        sys.exit(1)

    merge_yaml(sys.argv[1], sys.argv[2], sys.argv[3])

