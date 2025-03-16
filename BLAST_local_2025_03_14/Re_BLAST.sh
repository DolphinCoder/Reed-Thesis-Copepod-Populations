for i in Re_BLAST_in/*.fsa
  do
  echo "BLASTing ${i}"
  base=$(basename $i .fsa)
  blastn -db nt -query "${i}" -out "Re_BLAST_out/${base}_results.out" -max_target_seqs 5 -outfmt "6 ssciname sscinames pident" -remote &
  sleep 120
  echo "${i} in background"
done
