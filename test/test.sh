#!/bin/bash
oscar_compile=./oscar
TEST_DIR=$(pwd)

case_passed=0
case_failed=0

errorFile=errors.log
compiler_error_flag=0

# Set time limit for all operations
ulimit -t 30

show_result() {
  if [ $? -eq 0 ]; then
    echo -e "$2 passed$" >> session_file
    echo -e "$2 passed$"
    ((case_passed++))

  else
    echo -e "$2 failed$" >> session_file
    echo -e "$2 failed$"

    #print out expected output and result

    if [ $compiler_error_flag -eq 0 ]; then
      cat $1$2 >> session_file
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

test_scanner() {
  echo "***********************************************" >> session_file
  echo "Oscar Test Script Results:" >> session_file


  for oscar_test_file in $1*.oscar ; do
    filename=$(basename "$oscar_test_file")

    echo "Testing Scanner: $filename" >> session_file
    echo "==================================" >> session_file

    #Create file to be tested (with tokens)
    # $oscar_compile -s "$oscar_test_file" > oscar_test_output

    #Test output differences use the diff command and neglect screen output
    diff $oscar_test_output $oscar_test_file$testExtension > /dev/null

    show_result $1 $filename $scanner_extension
  done
}

test_compiler() {
  # The following portion is only to test compiler errors
  # set flag to prevent
  # compiler_error_flag=1

  for oscar_test_file in $1*.oscar ; do
    filename=$(basename "$oscar_test_file")

    echo "==================================" >> session_file
    echo "Testing Compiler: $filename" >> session_file

    # echo -e "${CYAN}Dice Compiler Messages (if any):" >> session_file
    #run the executable and port output (stderr) to temp test file

    #port stdout (compiler msgs) to log file
    $oscar_compile $1$oscar_test_file 2> temp.ll 1>> session_file
    echo -e "${NC}">> session_file
    echo "" >> session_file

    #run the executable and port error  output (stdout) to temp test file
    #port stdout (compiler msgs) to log file
    $oscar_compile "$oscar_test_file" 1> oscar_test_output 2>/dev/null

    #Perform comparison of outputs
    #diff oscar_test_output "$test_path"$filename$testExtension >> /dev/null
    show_result $1 filename $compiler_extension #function
  done

  echo "" >> session_file

  #Verbose flag actuated

  cat session_file
  cat session_file >> "$logFile"

  #Test status output
  echo ""
  echo -e "Tests Passed: $case_passed $"
  echo -e "Tests Failed: $case_failed $"

  #Clean up temp files
  rm oscar_test_output;
  rm session_file;
}

make_oscar(){
  echo "Oscar Compiler"
  cd ../
  make
  echo "Oscar Compiler created"
}

#-----------Script starts flag checking here ------------------

echo "Oscar test started"
make_oscar

logFile=./logfile.log

test_path="$TEST_DIR"/oscar/scanner/
scanner_extension=.scan
test_scanner $test_path $scanner_extension

# test_path=oscar/compiler/
# compiler_extension=.out
# test_scanner $test_path $compiler_extension

# rm temp.ll;

# errorLines=$(cat $errorFile | wc -l)
# mv $errorFile Test\ Suite/$errorFile

# if [ $errorLines -ne 0 ]; then
#   echo "$errorLines lines of script errors reported. Please check $errorFile!"
# else
#   mv Test\ Suite/$errorFile
# fi

exit 0
