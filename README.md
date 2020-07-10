# Check and match genotype data
In this repo we host some tools that we use to evaluate genotype data that we generate at CGI. Most of the tools is geared towards the data set we generate in monitoring wildlife populations in Sweden. The focus is on making it simple to match new genotype data with data that are available in databases. 

## Steps to deploy app in a shiny-server environment:
1. `git clone https://github.com/CGI-NRM/GenotypeCheck/`
2. `cd GenotypeCheck`
3. `git checkout develop`
4. `sudo -i R`
5. `setwd("path/to/dir/GenotypeCheck/")`
6. `devtools::install()`
7. `cp R/app.R srv/shiny-server/app.r`
8. Edit the `database_path` and `database_file` variables in app.R to point towards the file with the dataset. It can load an excel or ods file but will write a csv file on save and will therefore load the original data repeatedly if the file path is not replaced to point to the saved csv. A sqlite3 database is prefered over csv files. Backups of the data is made on save, which backups that will be kept over time is configured in the variable `keep_files` in app.R. 

##### If the `database_path` and `database_file` are empty the user will be asked to upload a dataset to the shiny app for every use. The backups will be written in the ~/Downloads/ folder and NO BACKUP REMOVAL will be performed. 

## Setup of datasets
### CSV File
#### Columns: 
`index` , `date` , `north` , `east` , `gender` , `date_changed` , `confirmed_dead` , `individ` , `G10L_1` , `G10L_2` , `MU05_1` , `MU05_2` , `MU09_1` , `MU09_2` , `MU10_1` , `MU10_2` , `MU23_1` , `MU23_2` , `MU50_1` , `MU50_2` , `MU51_1` , `MU51_2` , `MU59_1` , `MU59_2`
#### Conditions:
`index` - Unique. Max 10 characters long
`date` - Date in the format of `yyyy-mm-dd`
`north` - Integer (SWEREF99)
`east` - Integer (SWEREF99)
`gender` - Prefered to keep consistent, do not mix styles.
`date_changed` - Date in the format of `yyyy-mm-dd hh:mm:ss`
`confirmed_dead` - "Yes" or "No"
`individ` - Max 8 characters long
`all locus columns` - Integer. "0" or "NA" for missing data. Max 3 characters long

### SQLite database
This assumes you have a csv-file named bears.csv formated according to the above. 
1. `sqlite3 bears.db`
2. `.mode csv`
3. `.import bears.csv Bears_temp`
4. `CREATE TABLE Bears('index' CHAR(10) PRIMARY KEY NOT NULL, 'individ' CHAR(8), 'gender' CHAR(4), 'date' CHAR(10), 'north' INT, 'east' INT, 'MU09_1' CHAR(3), 'MU09_2' CHAR(3), 'MU10_1' CHAR(3), 'MU10_2' CHAR(3), 'MU05_1' CHAR(3), 'MU05_2' CHAR(3), 'MU23_1' CHAR(3), 'MU23_2' CHAR(3), 'MU51_1' CHAR(3), 'MU51_2' CHAR(3), 'MU59_1' CHAR(3), 'MU59_2' CHAR(3), 'G10L_1' CHAR(3), 'G10L_2' CHAR(3), 'MU50_1' CHAR(3), 'MU50_2' CHAR(3), 'confirmed_dead' CHAR(3), 'date_changed' CHAR(19));`
5. `INSERT INTO Bears ('index', 'individ', 'gender', 'date', 'north', 'east', 'MU09_1', 'MU09_2', 'MU10_1', 'MU10_2', 'MU05_1', 'MU05_2', 'MU23_1', 'MU23_2', 'MU51_1', 'MU51_2', 'MU59_1', 'MU59_2', 'G10L_1', 'G10L_2', 'MU50_1', 'MU50_2', 'confirmed_dead', 'date_changed') SELECT "index", "individ", "gender", "date", "north", "east", "MU09_1", "MU09_2", "MU10_1", "MU10_2", "MU05_1", "MU05_2", "MU23_1", "MU23_2", "MU51_1", "MU51_2", "MU59_1", "MU59_2", "G10L_1", "G10L_2", "MU50_1", "MU50_2", "confirmed_dead", "date_changed" FROM Bears_temp;`
6. `DROP TABLE Bears_temp;`

