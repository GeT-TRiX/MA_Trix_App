#!/bin/bash

{
  echo "City, Tmin, Tmax, Date, Tmin1, Tmax1"
  join -t, <(sort All_topTableAll.csv) <(sed 1d *WorkingSet.csv | sort)
} > d03.csv

