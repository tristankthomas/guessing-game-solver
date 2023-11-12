import os
import pandas as pd

input_folder = './tests'
file_statistics = {}

# iterate through all test csvs
for filename in os.listdir(input_folder):
    file_path = os.path.join(input_folder, filename)

    df = pd.read_csv(file_path, header=None, names=['Line', 'Data'])

    # convert value to int
    df['Data'] = df['Data'].astype(int)

    # calculate mean and standard deviation
    mean = df['Data'].mean().round(4)
    stddev = df['Data'].std().round(4)
    min_value = df['Data'][df['Data'] != 1].min()
    max_value = df['Data'].max()
    cv = stddev / mean.round(4)

    # extract the part of the filename for the row index (e.g., 'A3 A4 C2')
    row_index = os.path.splitext(filename)[0].replace('results_', '')

    file_statistics[row_index] = {'Mean': mean, 'StdDev': stddev, 'CV': cv, 'Min': min_value, 'Max': max_value}

# create a DataFrame from the dictionary
result_df = pd.DataFrame.from_dict(file_statistics, orient='index')

sorted_df = result_df.sort_values(by=['Mean', 'StdDev'])

output_csv = 'results_official.csv'
sorted_df.to_csv(output_csv)
