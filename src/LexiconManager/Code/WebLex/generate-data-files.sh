#!/bin/bash

trips_src_dir=$1
lexicon_data_dir=$2

echo "Using the TRIPS source directory $trips_src_dir"
echo "to update the lexicon data directory $lexicon_data_dir ..."

echo "  removing old data files ..."
# argument list too long :(
#rm $lexicon_data_dir/{ONT,W}\:\:*.xml
rm $lexicon_data_dir/ONT\:\:[0-9]*.xml
rm $lexicon_data_dir/ONT\:\:[a-m]*.xml
rm $lexicon_data_dir/ONT\:\:[n-z]*.xml
rm $lexicon_data_dir/W\:\:[0-9]*.xml
rm $lexicon_data_dir/W\:\:[a-e]*.xml
rm $lexicon_data_dir/W\:\:[f-j]*.xml
rm $lexicon_data_dir/W\:\:[k-o]*.xml
rm $lexicon_data_dir/W\:\:[p-t]*.xml
rm $lexicon_data_dir/W\:\:[u-z]*.xml
rm $lexicon_data_dir/W\:\:[\<^.]*.xml

rm -f $lexicon_data_dir/TRIPS-ontology.xml
rm -f $lexicon_data_dir/trips-ont.omn

echo "  generating new data files ..."
# CMUCL
#cat - <<EOP | lisp -quiet -batch
#:cd "$trips_src_dir/Parser"
#(load "parser")
#:cd "$trips_src_dir/LexiconManager/Code/WebLex/"
# SBCL
cat - <<EOP | lisp --noinform --noprint
(setf *default-pathname-defaults* #P"$trips_src_dir/Parser/")
(load "parser")
(setf *default-pathname-defaults* #P"$trips_src_dir/LexiconManager/Code/WebLex/")

(load "lisp2xml")
(load "onttypes2xml")
(load "words2xml")
(format t "(write-all-ont-type-xmls \"$lexicon_data_dir\") ...~%")
(write-all-ont-type-xmls "$lexicon_data_dir")
(format t "(write-all-word-xmls \"$lexicon_data_dir\") ...~%")
(write-all-word-xmls "$lexicon_data_dir")
(format t "(write-single-ontology-xml \"$lexicon_data_dir\") ...~%")
(write-single-ontology-xml "$lexicon_data_dir")
(format t "; done with lisp processing.~%")
EOP
# lisp doesn't print a newline when it quits :(
echo

lisp --load make-trips-ont-owl.lisp --quit
mv trips-ont.omn $lexicon_data_dir/

# Filter the newly created files to change the "modified" field from UNIX 
# timestamp to human-readable time
echo "  making 'modified' attribute a human-readable time ..."
# "argument list too long" :(
#perl -i -p -e 's/modified="(\d*)"/"modified=\"" . localtime($1) . "\""/e;' $lexicon_data_dir/{ONT,W}\:\:*.xml
perl -i -p -e 's/modified="(\d*)"/"modified=\"" . localtime($1) . "\""/e;' $lexicon_data_dir/ONT\:\:[0-9]*.xml
perl -i -p -e 's/modified="(\d*)"/"modified=\"" . localtime($1) . "\""/e;' $lexicon_data_dir/ONT\:\:[a-m]*.xml
perl -i -p -e 's/modified="(\d*)"/"modified=\"" . localtime($1) . "\""/e;' $lexicon_data_dir/ONT\:\:[n-z]*.xml
perl -i -p -e 's/modified="(\d*)"/"modified=\"" . localtime($1) . "\""/e;' $lexicon_data_dir/W\:\:[0-9]*.xml
perl -i -p -e 's/modified="(\d*)"/"modified=\"" . localtime($1) . "\""/e;' $lexicon_data_dir/W\:\:[a-e]*.xml
perl -i -p -e 's/modified="(\d*)"/"modified=\"" . localtime($1) . "\""/e;' $lexicon_data_dir/W\:\:[f-j]*.xml
perl -i -p -e 's/modified="(\d*)"/"modified=\"" . localtime($1) . "\""/e;' $lexicon_data_dir/W\:\:[k-o]*.xml
perl -i -p -e 's/modified="(\d*)"/"modified=\"" . localtime($1) . "\""/e;' $lexicon_data_dir/W\:\:[p-t]*.xml
perl -i -p -e 's/modified="(\d*)"/"modified=\"" . localtime($1) . "\""/e;' $lexicon_data_dir/W\:\:[u-z]*.xml
perl -i -p -e 's/modified="(\d*)"/"modified=\"" . localtime($1) . "\""/e;' $lexicon_data_dir/W\:\:[\<^.]*.xml

echo "done."

