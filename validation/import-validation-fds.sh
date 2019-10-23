#!/bin/bash

#Local copy of FDS git repository
#git clone git@github.com:firemodels/fds.git

i=0

#Importing Validation tests from FDS
for d in fds/Validation/*/ ; do
    echo "Found new Validation case: $d"
    case_name=$(basename $d)

    cases+=( $case_name )

    i=$i+1

    echo "case: $case_name"
    echo "case: ${cases[$@]}"

    #Removing test if present
    if [ -d $case_name ]
    then
       echo "REMOVE: $case_name"
       rm -rf $case_name
    fi

    #Copying test + creation of test.xml file
    # N.B.: to avoit XML parser installation we use echo
#    cp -r $d .
#    rm $case_name/*.sh
#
#    echo '<?xml version="1.0"?>'      > $case_name/test.xml 
#    echo '<testType>'                >> $case_name/test.xml
#    echo '   <blnfds>false</blnfds>' >> $case_name/test.xml 
#    echo '   <fdsfds>true</fdsfds>'  >> $case_name/test.xml 
#    echo '</testType>'               >> $case_name/test.xml 
done

#Removing FDS repository
#rm -rf fds
