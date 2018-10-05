#!/bin/bash
# Generates time table data and runs the output against the database

stack build

FILES="SVRFSACM05.xml SVRFSACM25.xml SVRFSACM23.xml SVRFSACM03.xml SVRFSACM18.xml"

for FILE in $FILES
do
    stack exec parser-exe "$FILE"
done

cat output/table_generation.sql output/*.xml.sql output/post_updates.sql > output/departures.sql

# add to database
sqlite3 output/departures.sqlite < output/departures.sql
