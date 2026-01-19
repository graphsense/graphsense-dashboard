import argparse
import csv

import yaml

def yaml_to_csv(yaml_file_path, csv_file_path):
    # Read the YAML file
    with open(yaml_file_path, 'r') as yaml_file:
        data = yaml.safe_load(yaml_file)

    # Write to CSV file
    with open(csv_file_path, 'w', newline='') as csv_file:
        writer = csv.writer(csv_file)
        writer.writerow(['Key', 'Value'])  # Write header
        for key, value in data.items():
            writer.writerow([key, value])  # Write each key-value pair as a row


def csv_to_yaml(csv_file_path, yaml_file_path):
    """
    Convert a CSV file with two columns (key-value pairs) to YAML.

    Args:
        csv_file_path (str): Path to the input CSV file.
        yaml_file_path (str): Path to the output YAML file.
    """
    data = {}

    with open(csv_file_path, mode='r') as csv_file:
        csv_reader = csv.reader(csv_file)
        for row in csv_reader:
            if len(row) == 2:  # Ensure the row has exactly two columns
                key, value = row
                data[key.strip()] = value.strip()

    with open(yaml_file_path, mode='w') as yaml_file:
        yaml.dump(data, yaml_file, default_flow_style=False, allow_unicode=True,
                 sort_keys=False)

def main():
    # Set up argument parser
    parser = argparse.ArgumentParser(description='Convert YAML to CSV.')
    parser.add_argument('from_file', help='Path to the input YAML/CSV file')
    parser.add_argument('to_file', help='Path to the output YAML/CSV file')

    # Parse arguments
    args = parser.parse_args()

    if args.from_file.endswith('.yaml') and args.to_file.endswith('.csv'):
        # Convert YAML to CSV
        yaml_to_csv(args.from_file, args.to_file)

    elif args.from_file.endswith('.csv') and args.to_file.endswith('.yaml'):
        # Convert YAML to CSV
        csv_to_yaml(args.from_file, args.to_file)

    else:
        print('one must end with .yaml, the other with .csv')

if __name__ == '__main__':
    main()
