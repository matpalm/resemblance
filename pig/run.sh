set -x
rm output.10 
pig -x local -p input=name_addr.10 -p output=output.10 resemblance.pig 