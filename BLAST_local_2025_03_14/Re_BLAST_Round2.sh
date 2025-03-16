for line in $(cat Round_2_names.txt); 
  do
  string="${line}"
  suffix="_results.out"
  base=${string%"$suffix"}
  echo "${base}"
  blastn -db nt -query "Re_BLAST_in/"${base}".fsa"  -out "Re_BLAST_out/${base}_results.out" -max_target_seqs 5 -outfmt "6 ssciname sscinames pident" -remote &
  sleep 120
  done
