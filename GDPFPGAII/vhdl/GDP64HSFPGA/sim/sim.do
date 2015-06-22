
set StdArithNoWarnings 1

#when -label end_of_simulation {eos == '1'} {echo "End of simulation" ; stop ;}

run 10

set StdArithNoWarnings 1

run -a