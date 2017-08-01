#!/bin/bash

stack build
stack exec parser-exe "SVRFSACM05.xml"
stack exec parser-exe "SVRFSACM05A.xml"

cat output/table_generation.sql output/*.xml.sql > output/timetable.sql

# add to database
sqlite3 output/timetables.sqlite < output/timetable.sql