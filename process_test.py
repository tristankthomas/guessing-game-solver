import pandas as pd
import matplotlib.pyplot as plt

# iterate through files in the input folder
file_path = './tests_official/results_B1 H2 H4.txt'
df = pd.read_csv(file_path, header=None, names=['Line', 'Data'])

# convert value to int
df['Data'] = df['Data'].astype(int)

# calculate statistics
mean = df['Data'].mean().round(4)
stddev = df['Data'].std().round(4)
min_value = df['Data'][df['Data'] != 1].min()
max_value = df['Data'].max()
cv = stddev / mean.round(4)

value_counts = pd.Series(df['Data']).value_counts().sort_index()

# get unique values and frequencies
unique_values = value_counts.index.tolist()
frequencies = value_counts.tolist()

print(f"Average of guesses: {mean:.4f}")
print(f"Standard Deviation of guesses: {stddev:.4f}")
print(f"Minimum guess count: {min_value}")
print(f"Maximum guess count: {max_value}")
plt.bar(unique_values, frequencies, color='b', edgecolor='black', tick_label=unique_values)

plt.savefig('histogram.png')