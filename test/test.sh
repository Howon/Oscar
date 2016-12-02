#!/bin/bash

# Path to the LLVM interpreter
#LLI="lli"
LLI="/usr/local/opt/llvm/bin/lli"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color


# Globals
oscar_compile=../oscar
TEST_DIR=$(pwd)

case_passed=0
case_failed=0

errorFile=errors.log
compiler_error_flag=0

# Set time limit for all operations
ulimit -t 30


show_result() {
  if [ $? -eq 0 ]; then
    echo -e "$2 passed" >> session_file
    echo -e "${GREEN}$2 passed${NC}"

    echo "" >> session_file
    echo ""

    ((case_passed++))

  else
    echo -e "$2 failed $" >> session_file
    echo -e "${RED}$2 failed${NC}"

    echo ""

    #print out expected output and result

    if [ $compiler_error_flag -eq 0 ]; then
      cat $1$2$4 >> session_file
    else
      cat $1$2$3 >> session_file
    fi

    echo "" >> session_file
    echo "Generated Output:" >> session_file
    cat oscar_test_output  >> session_file
    echo "" >> session_file

    ((case_failed++))
  fi
}

# test pretty printing of AST
test_scanner() {
  echo "***********************************************" >> session_file
  echo "Oscar Test Script Results:" >> session_file


  for oscar_test_file in $1*.oscar ; do
    filename=$(basename "$oscar_test_file")

    echo "Testing Scanner: $filename" >> session_file
    echo "==================================" >> session_file

    # Create file to be tested (with tokens)
    $oscar_compile -a < $oscar_test_file > oscar_test_output

    #Test output differences use the diff command and neglect screen output
    # diff $oscar_test_output $oscar_test_file$compiler_extension > /dev/null

    show_result $1 $filename $scanner_extension
  done
}

# test files and make sure we get the right outputs
test_compiler() {
  # set flag to prevent
  # compiler_error_flag=1

  for oscar_test_file in $1*.oscar ; do
    filename=$(basename "$oscar_test_file")
    filename="${filename%.*}"

    echo "==================================" >> session_file
    echo "Testing Compiler: $filename" >> session_file


    # compile program to test.ll, put any errors in the session
    echo -e "Oscar Compiler Messages:" >> session_file
    $oscar_compile < $oscar_test_file 1> temp.ll 2>> session_file
    echo "" >> session_file

    # run lli on the file and save the output and errors
    echo -e "LLVM Messages:" >> session_file
    $LLI temp.ll 1> oscar_test_output 2>> session_file

    echo "" >> session_file

    # Perform comparison of outputs
    # why is this being done tho lol
    #cat oscar_test_output
    #cat "$test_path"$filename$compiler_extension
    #diff oscar_test_output "$test_path"$filename$compiler_extension
    diff oscar_test_output "$test_path"$filename$compiler_extension >> /dev/null
    show_result $1 $filename $compiler_extension $test_extension
  done

  echo "" >> session_file

  #Verbose flag actuated

  # cat session_file
  cat session_file >> "$logFile"

  #Test status output
  echo ""
  echo -e "Tests Passed: $case_passed $"
  echo -e "Tests Failed: $case_failed $"

  #Clean up temp files
  rm -f oscar_test_output;
  rm -f session_file;
  rm -f temp.ll;
}

make_oscar(){
  echo "Oscar Compiler"
  cd ../
  make
  echo "Oscar Compiler created"
  echo ""
  cd test/
}


#-----------Script starts flag checking here ------------------

echo "Oscar test started"
make_oscar

logFile=./logfile.log
echo "" > $logFile

#test_path="$TEST_DIR"/oscar/scanner/
#scanner_extension=.scan

# test_scanner $test_path $scanner_extension

test_path=oscar/compiler/
test_extension=.oscar
compiler_extension=$test_extension.out
test_compiler $test_path $compiler_extension $test_extension

rm -f ../oscar


# errorLines=$(cat $errorFile | wc -l)
# mv $errorFile Test\ Suite/$errorFile

# if [ $errorLines -ne 0 ]; then
#   echo "$errorLines lines of script errors reported. Please check $errorFile!"
# else
#   mv Test\ Suite/$errorFile
# fi

exit 0