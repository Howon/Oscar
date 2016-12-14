#!/bin/bash

# Path to the LLVM interpreter
LLI="lli"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color


# Globals
oscar_compile="./oscar -c"
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
    $oscar_compile -s $oscar_test_file > oscar_test_output

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
    $oscar_compile $oscar_test_file 1> temp.ll 2> oscar_error_output

    

    echo "" > oscar_test_output

    # if we had a error, then diff errors. Otherwise, run LLVM and diff outputs
    if [ -s oscar_error_output ]; then
      echo "" >> oscar_error_output
      cat oscar_error_output >> session_file
      echo "" >> session_file

       # diff errors
      diff oscar_error_output "$test_path"$filename$compiler_extension >> /dev/null
      show_result $1 $filename $compiler_extension $test_extension

    else

      # run lli on the file and save the output and errors
      echo -e "LLVM Messages:" >> session_file
      $LLI temp.ll 1> oscar_test_output 2>> session_file
      echo "" >> session_file

      # diff outputs
      diff oscar_test_output "$test_path"$filename$compiler_extension >> /dev/null
      show_result $1 $filename $compiler_extension $test_extension

    fi

    done

  echo "" >> session_file
}

make_oscar(){
  echo "Oscar Compiler"
  cd ../
  make
  echo "Oscar Compiler created"
  echo ""
}


#-----------Script starts flag checking here ------------------

echo "Oscar test started"
make_oscar

logFile=./test/logfile.log
echo "" > $logFile

echo -e "\n\n${CYAN}----------Testing Valid----------${NC}\n"
test_path=./test/oscar/compiler/
test_extension=.oscar
compiler_extension=$test_extension.out
test_compiler $test_path $compiler_extension $test_extension

echo -e "\n\n${CYAN}----------Testing Errors----------${NC}\n"
# testing errors
test_path=./test/oscar/compiler/errors/
test_compiler $test_path $compiler_extension $test_extension

# cat session_file
cat session_file >> "$logFile"

#Test status output
echo ""
echo -e "Tests Passed: $case_passed $"
echo -e "Tests Failed: $case_failed $"

#Clean up temp files
rm -f oscar_test_output;
rm -f oscar_error_output
rm -f session_file;
rm -f temp.ll;

make clean
cd test
exit 0
