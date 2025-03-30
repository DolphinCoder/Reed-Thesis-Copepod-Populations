for line in $(cat Round_3_FINAL.txt); 
  do
  string="${line}"
  suffix="_results.out"
  base=${string%"$suffix"}
  echo "${base}"
  /media/volume/Ella-Thesis/BLAST/ncbi-blast-2.16.0+/bin/blastn -db nt -query "Re_BLAST_in/"${base}".fsa"  -out "Re_BLAST_out/${base}_results.out" -max_target_seqs 5 -outfmt "6 ssciname sscinames pident" -num_threads 32 &
  sleep 30
  outfile="Re_BLAST_out/${base}_results.out"
  echo "${outfile}" 
  done

echo "it is done. may i please rest now."
