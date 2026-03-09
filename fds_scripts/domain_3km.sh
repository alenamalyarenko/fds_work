# run_name SOURCE_name IBAR JBAR KBAR NT I_UPPER J_UPPER XI Y1 Z1 T1
#cut out from palm:
#./make_files_per_domain.sh palm_c5 60 60 120 1000 2 2 102 102 2 1

#cut whole fds domain:
#./make_files_per_domain_fds.sh vali_s5 60 60 120 900 2 2 102 102 2 1

#cut partial fds domain:
./make_files_per_domain_fds.sh vali_s5 OUT_STD_V_1_5_full.nc 32 32 60 900 0 0  74 74 1 1

