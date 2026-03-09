#!/bin/bash
set -e  # exit on error

# Save original working directory
orig_dir=$(pwd)

# Ensure we always return to original directory on exit
trap 'cd "$orig_dir"' EXIT

# Track start time
start_time=$(date +%s)

# --- Help message function ---
show_help() {
cat << EOF
Usage: $(basename "$0") RUNNAME IBAR JBAR KBAR NT I_UPPER J_UPPER X1 Y1 Z1 T1

Generates FDS initial and boundary condition NetCDF files from PALM data.

Arguments:
  RUNNAME      Name of the simulation run
  IBAR         Domain size in x-direction
  JBAR         Domain size in y-direction
  KBAR         Domain size in z-direction
  NT           Number of timesteps
  I_UPPER      Upper halo size in x
  J_UPPER      Upper halo size in y
  X1           PALM cutout x-index start
  Y1           PALM cutout y-index start
  Z1           PALM cutout z-index start
  T1           PALM cutout time index start

Example:
  $(basename "$0") case01 60 60 120 1000 2 2 102 102 1 1

Options:
  -h, --help   Show this help message and exit

EOF
}
# --- Handle help or missing arguments ---
if [[ "$1" == "-h" || "$1" == "--help" || $# -lt 11 ]]; then
    show_help
    exit 0
fi



runname="$1"
#Domain Data:
#IBAR=60, JBAR=60, KBAR=120, NT=1000 !601
#I_UPPER=2, J_UPPER=2
IBAR="$2"           
JBAR="$3"           
KBAR="$4"
NT="$5"
I_UPPER="$6"
J_UPPER="$7"

#Palm cutout data: - not coded yet!!!!!
#x1=102, y1=102, z1=2, t1=1 
X1="$8"
Y1="$9"
Z1="$10"
T1="$11"

# --- Work directory setup ---
workdir="/mnt/data2/data2/FDS/fds_work/fds_scripts"
datadir="/mnt/data2/data2/FDS/PALM_data_for_FDS"


logfile="$workdir/run_${runname}_${mode}_$(date '+%Y%m%d_%H%M%S').log"
# Redirect all output to screen + logfile
exec > >(tee -a "$logfile") 2>&1





ulimit -s unlimited

rm -f program1.exe program2.exe

ifx -mcmodel=medium  -o program1.exe  make_ic_from_palm_domain.f90 -I/usr/local/netcdf-ifort/4.6.1/include -I/usr/local/netcdf-ifort/4.6.1/include -L/usr/local/netcdf-ifort/4.6.1/lib -lnetcdff -shared-intel -L/usr/lib -I/usr/include -lnetcdf -lm
ifx -mcmodel=medium  -o program2.exe  make_bc_from_palm_single_domain.f90 -I/usr/local/netcdf-ifort/4.6.1/include -I/usr/local/netcdf-ifort/4.6.1/include -L/usr/local/netcdf-ifort/4.6.1/lib -lnetcdff -shared-intel -L/usr/lib -I/usr/include -lnetcdf -lm

cp program1.exe "$datadir"
cp program2.exe "$datadir"

cd "$datadir"
echo "Starting IC creation"

./program1.exe "$runname" "$IBAR" "$JBAR" "$KBAR" "$I_UPPER" "$J_UPPER" 


echo "Starting BC creation"


./program2.exe "$runname" "$IBAR" "$JBAR" "$KBAR" "$NT" "$I_UPPER" "$J_UPPER" "T"
./program2.exe "$runname" "$IBAR" "$JBAR" "$KBAR" "$NT" "$I_UPPER" "$J_UPPER" U
./program2.exe "$runname" "$IBAR" "$JBAR" "$KBAR" "$NT" "$I_UPPER" "$J_UPPER" V
./program2.exe "$runname" "$IBAR" "$JBAR" "$KBAR" "$NT" "$I_UPPER" "$J_UPPER" W

cdo merge "bc_${runname}_T.nc" "bc_${runname}_U.nc" "bc_${runname}_V.nc" "bc_${runname}_W.nc"   "bc_${runname}.nc"
cdo merge "bc_${runname}_T_10s.nc" "bc_${runname}_U_10s.nc" "bc_${runname}_V_10s.nc" "bc_${runname}_W_10s.nc"   "bc_${runname}_10s.nc"
cdo merge "bc_${runname}_T_1m.nc" "bc_${runname}_U_1m.nc" "bc_${runname}_V_1m.nc" "bc_${runname}_W_1m.nc"   "bc_${runname}_1m.nc"

mv "ic_${runname}.nc" "$workdir"
mv "bc_${runname}.nc" "$workdir"
mv "bc_${runname}_10s.nc" "$workdir"
mv "bc_${runname}_1m.nc" "$workdir"

# --- Runtime report ---
end_time=$(date +%s)
elapsed=$(( end_time - start_time ))
hours=$(( elapsed / 3600 ))
minutes=$(( (elapsed % 3600) / 60 ))
seconds=$(( elapsed % 60 ))

echo "--------------------------------------------------"
echo "[$(date '+%Y-%m-%d %H:%M:%S')] Run: $runname | Mode: ICBC"
echo "Total runtime: ${hours}h ${minutes}m ${seconds}s"
echo "Log saved to: $logfile"
echo "--------------------------------------------------"
