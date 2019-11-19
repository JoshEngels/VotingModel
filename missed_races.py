import csv

with open('data/single_layout.csv') as input, open('data/single_layout_summary.csv', 'w', newline='') as output:

    csv_reader = csv.reader(input, delimiter=',')
    csv_writer = csv.writer(output, delimiter=',')

    headers = ["raceSpace", "titleSpace", "candidateSpace", "iteration", "raceIndex", "raceLength", "column", "yPos", "votedOn"]
    csv_writer.writerow(headers)

    param_count = 0
    current_param = None
    for row in csv_reader:

        first_col, second_col, third_col, all_races, order, params = \
            [[int(e) for e in elem.strip().split(" ")] for elem in row]

        if params != current_param:
            current_param = params
            param_count = 0
        param_count += 1

        # race_col_info is a list of the races in the same order as all races (top to bottom left to right)
        # Each entry is the number of candidates in that race, the starting y position, and the col number (1, 2, 3)
        race_col_info = []
        for col_num, col in enumerate([first_col, second_col, third_col]):
            race_col_info.append((col[0], 10, col_num + 1))
            for i in range(1, len(col)):
                race_col_info.append(
                    (col[i],
                     race_col_info[-1][1] + params[1] + params[2] * race_col_info[-1][0] + params[0],
                     col_num + 1))

        # To ouput file: Params and number trial, race length, voted on -1 or number, col, position in column
        # Generate each one of these values
        for race_index, race_length in enumerate(all_races):

            voted_on = order.index(race_index) if race_index in order else -1
            col_info = race_col_info[race_index]

            row = [params[0], params[1], params[2], param_count, race_index, race_length,
                   col_info[2], col_info[1],
                   voted_on]
            csv_writer.writerow(row)

