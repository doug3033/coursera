The run_analysis.R scirpt first sets the R working directory to point to where the FUCI dataset (https://d396qusza40orc.cloudfront.net) has been unziped.  The setwd command can be changed to point to whereever this is on a local computer.

The FUCI dataset must be in the same structure as when it was unzipped with "test" and "train" subdirectories.

The program then sets up pointers to each file making up the dataset and then loads them into dataframes.  The dataframes used are:
			X.train - training data
			X.test - test data
			subject.test - subjects in the test data
			subject.train - subjects in the training data
			Y.test - activity (by id) in the test data
			Y.train - activity (by id) in the train data
			features - a list of the column descriptions for X. data
			activity.labels - a list of activity ids and associated labels

A "type" column is added to the dataframe to distinguish test from train data after the dataframes for each have been combined.

Next, the dataframes for the test data and training data are combined into a single dataframe: X.common. The subject and y dataframes are combined as well.

Next, the features are processed before adding them as names to the X.common dataframe.
		1) "_" are replaced with "."
		2) "(" and ")" are removed.

The columns of the X.common dataframe that have "mean" and "std" in their names are them selected out and placed into the X.common.select dataframe.

The y.common dataframe is merged with activity.labels so that a y.common.labeled dataframe contains both activity ids and labels.

The y.common.labeled and subject.common dataframes are them placed into the X.common.select dataframe.

Finally, averages are determined for every subject/activity in the X.common.select dataframe using ddplyr.  These averages are then written to "wearable_fitness_test_tidy.txt" on the working directory.
