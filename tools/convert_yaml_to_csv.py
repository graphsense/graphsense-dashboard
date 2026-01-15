import yaml
import csv
import argparse

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

def main():
    # Set up argument parser
    parser = argparse.ArgumentParser(description='Convert YAML to CSV.')
    parser.add_argument('yaml_file', help='Path to the input YAML file')
    parser.add_argument('csv_file', help='Path to the output CSV file')

    # Parse arguments
    args = parser.parse_args()

    # Convert YAML to CSV
    yaml_to_csv(args.yaml_file, args.csv_file)

if __name__ == '__main__':
    main()
