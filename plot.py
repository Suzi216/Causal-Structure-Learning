import matplotlib.pyplot as plt
import numpy as np

# # Your new execution time results
new_execution_times = [
    0.6510286, 0.4183879, 0.4068930, 1.4052110, 0.3361163, 0.4819942, 0.6405663, 4.4717274, 0.9268160,
    0.9318647, 1.1255724, 2.9153657, 0.9717970, 0.9280677, 1.4350536, 4.1103878, 1.1591291, 1.2998989,
    1.4777458, 5.7525206, 1.2773812, 0.9525456, 1.1828098, 5.0779755, 1.0690560, 1.0223501, 1.1532185,
    5.8125818, 0.3523343, 0.4024222, 0.5570881, 3.0733285, 0.5090852, 0.6419883, 1.2447357, 7.8947122,
    0.7200053, 0.8856490, 2.2772899, 8.6353700, 0.6210589, 0.5856326, 1.2445972, 9.7828033, 0.6802368,
    0.7690973, 1.2125289, 9.6352880, 1.0043449, 1.0128467, 2.8608162, 27.9974580, 1.0520592, 1.2514615,
    3.0048137, 26.3626721, 0.9929454, 1.2336588, 3.2255940, 23.6268237, 1.3434155, 1.4760258, 3.3560445,
    29.2405052
]

# Your new accuracy values
new_accuracy_values = [ 0.80, 0.78, 0.92, 0.82, 0.75, 0.70, 0.71, 0.65, 0.58, 0.68, 0.75, 0.76, 0.64, 0.67, 0.69, 0.54, 0.88, 0.82, 0.85, 0.83,
    0.62, 0.81, 0.77, 0.83, 0.65, 0.63, 0.73, 0.61, 0.69, 0.66, 0.61, 0.61, 0.88, 0.95, 0.91, 0.86, 0.80, 0.72, 0.82, 0.74,
    0.63, 0.70, 0.66, 0.67, 0.61, 0.46, 0.63, 0.69, 0.85, 0.88, 0.85, 0.85, 0.88, 0.81, 0.84, 0.84, 0.67, 0.74, 0.66, 0.73,
    0.70, 0.62, 0.64, 0.66]
new_accuracy_values = [value for value in new_accuracy_values]


# Split the new data into four groups, each containing 16 values
new_groups = [new_execution_times[i:i+16] for i in range(0, len(new_execution_times), 16)]

# Generate four bar charts in a 2x2 grid
fig, axs = plt.subplots(2, 2, figsize=(8, 5))

# Scaling factor for reducing the height of the bars
scaling_factor = 0.8
# Define labels for the top of each bar
labels = ['100', '1000', '10000', '100000'] * 4

# Define colors for each bar in the pattern green, darkgreen, brown, black
bar_colors = ['blue', 'darkgreen', 'brown', 'black']

# Define the values of n for each chart
ns = [10, 20, 50, 100]

for i, (group, n) in enumerate(zip(new_groups, ns)):
    row, col = divmod(i, 2)

    bar_positions = np.arange(16) + 0.1

    # Add more space between bars at specific indices
    # (you can adjust these values based on your preference)
    bar_positions[2] -= 0.2
    bar_positions[1] += 0.2
    bar_positions[0] += 0.6
    bar_positions[3] -= 0.6

    bar_positions[4] += 0.4
    bar_positions[6] -= 0.4
    bar_positions[7] -= 0.8


    bar_positions[8] += 0.5
    bar_positions[9] += 0.1
    bar_positions[10] -= 0.3
    bar_positions[11] -= 0.7

    bar_positions[12] += 0.3
    bar_positions[13] -= 0.1
    bar_positions[14] -= 0.5
    bar_positions[15] -= 0.9

        # Bar chart with primary y-axis (execution time)
    #   # Bar chart with primary y-axis (accuracy)
    for j, (bar, color) in enumerate(zip(bar_positions, bar_colors * 4)):
        axs[row, col].bar(bar, new_accuracy_values[i * 16 + j]  , width=0.5, color=color, alpha=0.7, label=f'{labels[j]} ms')

    # Line chart with secondary y-axis (execution time)
    axs_accuracy = axs[row, col].twinx()

    line_colors = ['blue', 'darkgreen', 'brown', 'black','blue', 'darkgreen', 'brown', 'black','blue', 'darkgreen', 'brown', 'black','blue', 'darkgreen', 'brown', 'black']
    axs_accuracy.scatter(bar_positions, group, c=line_colors, marker='o', alpha=0.5, label='Execution Time')
    axs_accuracy.plot(bar_positions, group, linestyle='-', color='gray', alpha=0.5)

    axs[row, col].set_title(f'n={n}', fontsize=10)
    axs[row, col].set_xlabel('Density', fontsize=8)
    axs[row, col].set_ylabel('Accuracy', fontsize=8)
    axs_accuracy.set_ylabel('Execution time (s)', fontsize=8)  # Set ylabel for the right axis
    axs[row, col].set_xticks(np.arange(0, 16, 4) + 1.5)
    axs[row, col].set_xticklabels(['2', '3', '4', '5'],fontsize=8)
    # Set fontsize for y-axis tick labels
    axs[row, col].tick_params(axis='y', labelsize=8)
    axs_accuracy.tick_params(axis='y', labelsize=8)


# Create a common legend for the colors outside the entire figure
handles = [plt.Line2D([0], [0], marker='o', color='w', markerfacecolor=color, markersize=12) for color in bar_colors]
fig.legend(handles, labels, loc='center left', bbox_to_anchor=(1, 0.5), title='Data Samples')

plt.tight_layout()
plt.show()
