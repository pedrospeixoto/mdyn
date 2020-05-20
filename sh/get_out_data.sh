find data/triples_br_2020_04_17_date/all/date0\=2020-* -name '*.csv' -type f  -exec cp --parents \{\} data_output/. \;
find data/triples_br_2020_04_17_date/all/date0\=2020-* -name '*.txt' -type f  -exec cp --parents \{\} data_output/. \;
find data/triples_br_2020_04_17_date/all/date0\=2020-* -name '*.pkl' -type f  -exec cp --parents \{\} data_output/. \;
