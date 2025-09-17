# Experiment 1 - Idealised, no fire
./makerun_uc.sh STD1_1 standalone
./makerun_uc.sh CPL1_0 coupled ic_palm_c1.nc bc_palm_smooth.nc
./makerun_uc.sh CPL1_1 coupled ic_palm_c1.nc bc_palm_c1.nc
./makerun_uc.sh CPL1_2 coupled ic_palm_c1.nc bc_palm_c1.nc
./makerun_uc.sh CPL1_3 coupled ic_palm_c1.nc bc_palm_c1_5s.nc
./makerun_uc.sh CPL1_4 coupled ic_palm_c1.nc bc_palm_c1_10s.nc
