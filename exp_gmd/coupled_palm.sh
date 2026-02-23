ulimit -s unlimited

#Coupled runs to PALM, no fire:
#1s
./makerun_uc.sh -m coupled -n 9 -i ic_palm_c4.nc -b bc_palm_c4.nc CPL1_19b
#10s
./makerun_uc.sh -m coupled -n 9 -i ic_palm_c4.nc -b bc_palm_c4_10s.nc CPL1_19
# 1m 
./makerun_uc.sh -m coupled -n 9 -i ic_palm_c4.nc -b bc_palm_c4_1m.nc CPL1_19d
#west only
./makerun_uc.sh -m coupled -n 9 -i ic_palm_c4.nc -b bc_palm_c4.nc CPL1_19c
#high res 
#./makerun_uc.sh CPL1_19e coupled ic_palm_c4.nc bc_palm_c4.nc

#Coupled runs to PALM, fire:
./makerun_uc.sh -m coupledsm -i ic_palm_c4.nc -b bc_palm_c4.nc -n 9 -p yes CPL1_19b_FIREsm
./makerun_uc.sh -m coupled -i ic_palm_c4.nc -b bc_palm_c4.nc -n 9 -p yes CPL1_19b_FIRE
