for i in Re_BLAST_in/*.fsa
  do
  echo "${i}"
  base=$(basename $i .fsa)
  echo "Re_BLAST_out/${base}_results.out"
done
