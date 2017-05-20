#!/bin/bash

stack build
stack exec parser-exe

cat output/table_generation.sql output/*.xml.sql > output/timetable.sql

# add to database
sqlite3 output/timetables.sqlite < output/timetable.sql