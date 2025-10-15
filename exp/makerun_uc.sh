#!/bin/bash
set -e  # exit on error

# Save original working directory
orig_dir=$(pwd)

# Ensure we always return to original directory on exit
trap 'cd "$orig_dir"' EXIT

# Track start time
start_time=$(date +%s)

# --- Help message ---
usage() {
    echo ""
    echo "Usage: $0 RUNNAME [MODE] [ICFILE] [BCFILE]"
    echo ""
    echo "Arguments:"
    echo "  RUNNAME   Name of the run (required)"
    echo "  MODE      standalone | coupled (default: standalone)"
    echo "  ICFILE    Initial condition file (required if MODE=coupled)"
    echo "  BCFILE    Boundary condition file (required if MODE=coupled)"
    echo ""
    echo "Examples:"
    echo "  $0 test1"
    echo "     -> runs 'test1' in standalone mode"
    echo ""
    echo "  $0 test2 coupled ic_palm_c1.nc bc_palm_c1_5s.nc"
    echo "     -> runs 'test2' in coupled mode with IC and BC files"
    echo ""
    exit 1
}

# --- Parse arguments ---
if [ $# -lt 1 ] || [ "$1" = "--help" ] || [ "$1" = "-h" ]; then
    usage
fi

runname="$1"
mode="${2:-standalone}"   # default = standalone
icfile="${3:-}"           # must be given in coupled mode
bcfile="${4:-}"           # must be given in coupled mode
path_to_file=$(echo "$runname" | tr 'A-Z' 'a-z')

# --- Work directory setup ---
workdir="/mnt/data2/data2/FDS/fds_work/test_runs_local/$path_to_file"

# Create directory if it doesn't exist
if [ ! -d "$workdir" ]; then
    echo "Directory $workdir does not exist. Creating it..."
    mkdir -p "$workdir"
fi
cd "$workdir"

rm -f "$runname"*

logfile="$workdir/run_${runname}_${mode}_$(date '+%Y%m%d_%H%M%S').log"
# Redirect all output to screen + logfile
exec > >(tee -a "$logfile") 2>&1


rm -f fds_impi_intel_linux_uc
ulimit -s unlimited


# --- Select executable & input file ---
if [ "$mode" = "standalone" ]; then
    exec_src="/mnt/data2/data2/FDS/fds_work/backup_exec/fds_impi_intel_linux_uc"
    fds_file="/mnt/data2/data2/FDS/fds_work/exp/standalone/${path_to_file}.fds"
elif [ "$mode" = "coupled" ]; then
    exec_src="/mnt/data2/data2/FDS/fds_work/backup_exec/fds_impi_intel_linux_uc_coupled2"
    fds_file="/mnt/data2/data2/FDS/fds_work/exp/coupled/${path_to_file}.fds"
else
    echo "Error: unknown mode '$mode'. Must be 'standalone' or 'coupled'."
    usage
fi

# --- Extract T_END from .fds file ---
if [ -f "$fds_file" ]; then
    T_END=$(grep -i "T_END" "$fds_file" | head -n 1 | sed -E 's/.*T_END *= *([0-9.]+).*/\1/')
    if [ -n "$T_END" ]; then
        echo "Detected T_END=$T_END (from $fds_file)"
    else
        echo "Warning: could not detect T_END in $fds_file"
    fi
else
    echo "Error: fds_file $fds_file not found"
    exit 1
fi


cp "$exec_src" fds_impi_intel_linux_uc

# --- Copy boundary files if coupled ---
if [ "$mode" = "coupled" ]; then
    if [ -z "$icfile" ] || [ -z "$bcfile" ]; then
        echo "Error: Coupled mode requires ICFILE and BCFILE."
        usage
    fi

    if [ ! -f "/mnt/data2/data2/FDS/fds_work/fds_scripts/input_3km_domain/$icfile" ]; then
        echo "Error: ICFILE '$icfile' not found in fds_scripts"
        exit 1
    fi
    if [ ! -f "/mnt/data2/data2/FDS/fds_work/fds_scripts/input_3km_domain/$bcfile" ]; then
        echo "Error: BCFILE '$bcfile' not found in fds_scripts"
        exit 1
    fi

    cp "/mnt/data2/data2/FDS/fds_work/fds_scripts/input_3km_domain/$icfile" ic_palm_c1.nc
    cp "/mnt/data2/data2/FDS/fds_work/fds_scripts/input_3km_domain/$bcfile" bc_palm_c1.nc
fi

# --- Run FDS! ---
mpirun -np 9 ./fds_impi_intel_linux_uc "$fds_file"

# --- Postprocessing step ---
echo "Simulation finished. Running postprocessing..."
cd /mnt/data2/data2/FDS/fds_work/fds_scripts

rm -f program.exe

ifx -mcmodel=medium  -o program.exe  glue_output.f90 -I/usr/local/netcdf-ifort/4.6.1/include -I/usr/local/netcdf-ifort/4.6.1/include -L/usr/local/netcdf-ifort/4.6.1/lib -lnetcdff -shared-intel -L/usr/lib -I/usr/include -lnetcdf -lm

cp program.exe "$workdir"
cd "$workdir"
./program.exe "$runname" "$T_END"
mv "OUT_${runname}.nc" /mnt/data2/data2/FDS/fds_work/fds_scripts/


# --- Runtime report ---
end_time=$(date +%s)
elapsed=$(( end_time - start_time ))
hours=$(( elapsed / 3600 ))
minutes=$(( (elapsed % 3600) / 60 ))
seconds=$(( elapsed % 60 ))

echo "--------------------------------------------------"
echo "[$(date '+%Y-%m-%d %H:%M:%S')] Run: $runname | Mode: $mode"
echo "ICFILE=${icfile:-none} | BCFILE=${bcfile:-none}"
echo "Total runtime: ${hours}h ${minutes}m ${seconds}s"
echo "Log saved to: $logfile"
echo "--------------------------------------------------"
