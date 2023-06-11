# Seminar_in_Statistics
Code and documentation for the seminar thesis in statistics (SoSe 23).

**Order of execution:**
1. Since the used data set is private, all needed .csv and .shp and .gpx files are not provided in this repository and should be downloaded separetely.
2. After that open file 'Seminar Thesis in Statistics Nevskii.R' and execute everything up to line 336.
3. Open file 'OSRM_documentation.txt' and follow the instructions for setting a docker container.
4. Execute file 'FetchDistanceMatrixCode.py'. On a server it would take a few seconds, but the execution on a small local machine could take some time due to the size of the spatial database. In case of trouble, two resulting files 'distance_matrix.csv' and 'durations_matrix.csv' with driving distances and times are provided in the folder 'Distances' in this repository.
5. Execute the rest from .R file. On a laptop it could take up to 4 minutes due to multiple LOO CV performed.

References to plots and tables in the Thesis (.pdf) are stated as comments within the .R file.
