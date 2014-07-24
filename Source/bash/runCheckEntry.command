#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd $DIR
cd ../..

echo "Type the cholera taxonomy ID you would like to check, followed by [ENTER]:"
read cid
# we are expecting an argument parameter which will come in as $1
Rscript Source/R/runCheckEntry.R $cid
