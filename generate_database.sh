#!/bin/bash

stack build
stack exec parser-exe "SVRFSACM05.xml"
stack exec parser-exe "SVRFSACM05A.xml"

cat output/table_generation.sql output/*.xml.sql output/post_updates.sql > output/departures.sql

# add to database
sqlite3 output/departures.sqlite < output/departures.sql