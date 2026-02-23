#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

#

# Base directories (change once here)
BASE="/mnt/data2/data2/FDS/fds_work"
SCRIPT_DIR="$BASE/fds_scripts"
EXEC_DIR="$BASE/backup_exec"
EXP_DIR="$BASE/exp_gmd"
RUNS_ROOT="$BASE/test_runs_gmd"

# Default runtime options (override via flags or env)
DEFAULT_MODE="standalone"   # standalone | coupled
DEFAULT_NPROC=9

DEFAULT_POSTPROCESS="yes"   # yes | no
postprocess="$DEFAULT_POSTPROCESS"

RUN_MODEL="yes"  # yes | no


# Tools (ensure these are on PATH or set full path)
MPIRUN_CMD="${MPIRUN_CMD:-mpirun}"
COMPILER_CMD="${COMPILER_CMD:-ifx}"   # Fortran compiler used for postprocessing
# End configuration
#############################################
# Track start time 
starting_time=$(date +%s)

# Save original directory and setup traps
orig_dir="$(pwd)"
cleanup_and_exit() {
    local exit_code=${1:-0}
    cd "$orig_dir" || true
    echo "Exiting with status $exit_code"
    exit "$exit_code"
}
trap 'cleanup_and_exit $?' EXIT
trap 'echo "Interrupted"; cleanup_and_exit 130' INT



# Utilities
timestamp() { date '+%Y-%m-%d %H:%M:%S'; }
log() { printf '[%s] %s\n' "$(timestamp)" "$*"; }
err() { printf '[%s] ERROR: %s\n' "$(timestamp)" "$*" >&2; }


# --- Help message ---
usage() {
    cat <<EOF
Usage: $0 [-m mode] [-i ICFILE] [-b BCFILE] [-n NPROC] [-d RUNS_ROOT] RUNNAME
  RUNNAME        (required) name of the run (used to find .fds and name outputs)
Options:
  -m mode        standalone | coupled   (default: $DEFAULT_MODE)
  -i ICFILE      initial condition file (required if mode=coupled)
  -b BCFILE      boundary condition file (required if mode=coupled)
  -n NPROC       number of MPI ranks (default: $DEFAULT_NPROC)
  -d RUNS_ROOT   root directory for runs (default: $RUNS_ROOT)
  -p yes|no    enable or disable postprocessing (default: yes)
  -h             show this help and exit
Examples:
  $0 test1
  $0 -m coupled -i ic_palm_c1.nc -b bc_palm_c1_5s.nc -n 9 test2
EOF
    exit 1
}

# Parse arguments
mode="$DEFAULT_MODE"
icfile=""
bcfile=""
nproc="$DEFAULT_NPROC"

while getopts ":m:i:b:n:d:p:h-:" opt; do
    case "$opt" in
        m) mode="$OPTARG" ;;
        i) icfile="$OPTARG" ;;
        b) bcfile="$OPTARG" ;;
        n) nproc="$OPTARG" ;;
        d) RUNS_ROOT="$OPTARG" ;;
        p) postprocess="$OPTARG" ;;
        h) usage ;;
        -)  # long options
            case "$OPTARG" in
                norun) RUN_MODEL="no" ;;
                *) usage ;;
            esac
            ;;
        *) usage ;;
    esac
done
shift $((OPTIND -1))

if [ $# -lt 1 ]; then
    err "RUNNAME is required."
    usage
fi
runname="$1"

if [[ "$postprocess" != "yes" && "$postprocess" != "no" ]]; then
    err "Invalid value for -p (must be yes or no)"
    usage
fi




# Derived variables
path_to_file="$(echo "$runname" | tr 'A-Z' 'a-z')"
workdir="${RUNS_ROOT%/}/${path_to_file}"
logdir="$workdir/logs"
mkdir -p "$logdir"
logfile="$logdir/run_${runname}_${mode}_$(date '+%Y%m%d_%H%M%S').log"


# Redirect all output to both stdout and logfile
# shellcheck disable=SC2064
exec > >(tee -a "$logfile") 2>&1

log "Starting run '$runname'"
log "Mode: $mode | NPROC: $nproc | Workdir: $workdir"
log "Base directories: BASE=$BASE, SCRIPT_DIR=$SCRIPT_DIR, EXEC_DIR=$EXEC_DIR, EXP_DIR=$EXP_DIR"





# --- Work directory setup ---
# Create and cd to workdir
if [ ! -d "$workdir" ]; then
    log "Creating workdir: $workdir"
    mkdir -p "$workdir"
fi
cd "$workdir" || { err "Cannot cd to $workdir"; exit 1; }

# Prevent glob expansion surprises
shopt -s nullglob


# Clean previous transient files (safe)
if [ "$RUN_MODEL" = "yes" ]; then
     log "Cleaning transient files (pattern: ${runname}.* fds_impi_intel_linux_uc program.exe)"
     rm -f -- "${runname}"* fds_impi_intel_linux_uc program.exe || true
fi


ulimit -s unlimited


# Select executable & .fds file
case "$mode" in
    standalone)
        exec_src="${EXEC_DIR%/}/fds_impi_intel_linux_nc"
        fds_file="${EXP_DIR%/}/standalone/${path_to_file}.fds"
        ;;
    coupled)
        exec_src="${EXEC_DIR%/}/fds_impi_intel_linux_nc_coupled"
        fds_file="${EXP_DIR%/}/coupled/${path_to_file}.fds"
        ;;
    coupledsm)
        exec_src="${EXEC_DIR%/}/fds_impi_intel_linux_nc_coupledsm"
        fds_file="${EXP_DIR%/}/coupled/${path_to_file}.fds"
        ;;    
    *)
        err "Unknown mode '$mode' (must be 'standalone' or 'coupled')"
        usage
        ;;
esac

if [ ! -f "$exec_src" ]; then
    err "Executable source not found: $exec_src"
    exit 1
fi
if [ ! -f "$fds_file" ]; then
    err ".fds file not found: $fds_file"
    exit 1
fi



# --- Extract T_END from .fds file ---
if [ -f "$fds_file" ]; then
    T_END=$(awk -F= '
        BEGIN { IGNORECASE=1 }
        /T_END/ {
            # remove spaces, tabs
            val=$2
            gsub(/[ \t]/,"",val)
            # extract numeric part at start
            match(val, /^[0-9]+(\.[0-9]*)?/)
            print substr(val, RSTART, RLENGTH)
            exit
        }
    ' "$fds_file")


    if [ -n "$T_END" ]; then
        echo "Detected T_END=$T_END (from $fds_file)"
    else
        echo "Warning: could not detect T_END in $fds_file"
        T_END=0   # fallback to 0 for safety
    fi
else
    echo "Error: fds_file $fds_file not found"
    exit 1
fi

# Copy executable to working dir
log "Copying executable from $exec_src to $workdir"
cp -p "$exec_src" ./fds_impi_intel_linux_uc
chmod +x ./fds_impi_intel_linux_uc


# If coupled mode, check and copy ic/bc into working dir
if [[ "$mode" == "coupled" || "$mode" == "coupledsm" ]]; then
    if [ -z "$icfile" ] || [ -z "$bcfile" ]; then
        err "Coupled mode requires ICFILE (-i) and BCFILE (-b)."
        usage
    fi
    ic_src="${SCRIPT_DIR%/}/input_3km_domain/${icfile}"
    bc_src="${SCRIPT_DIR%/}/input_3km_domain/${bcfile}"
    if [ ! -f "$ic_src" ]; then err "IC file not found: $ic_src"; exit 1; fi
    if [ ! -f "$bc_src" ]; then err "BC file not found: $bc_src"; exit 1; fi
    cp -p "$ic_src" ic_palm_c1.nc
    cp -p "$bc_src" bc_palm_c1.nc
    log "Copied IC and BC to working directory"
fi

echo $RUN_MODEL
# --- Run FDS! ---
if [ "$RUN_MODEL" = "yes" ]; then
    log "Launching FDS with $MPIRUN_CMD -np $nproc ./fds_impi_intel_linux_uc $fds_file"
    run_start_epoch=$(date +%s)
    if ! "$MPIRUN_CMD"  -np "$nproc" ./fds_impi_intel_linux_uc "$fds_file"; then
        err "FDS run failed."
        exit 1
    fi
    run_end_epoch=$(date +%s)
    log "FDS run completed"
else
    log "Skipping FDS model run (-norun flag set)"
fi


# --- Postprocessing step ---

if [ "$postprocess" = "yes" ]; then
    log "Postprocessing enabled"
    
    # --- Extract grid variables from the original namelist --------------------
    namelist_file="$fds_file"

    if [ ! -f "$namelist_file" ]; then
        err "Namelist not found: $namelist_file"
    else
        # --- Extract IBAR, JBAR, KBAR from &MESH ---
        # Read the &MESH line
        mesh_line=$(grep -i '&MESH' "$namelist_file")
        
        # Extract IBAR, JBAR, KBAR using Bash regex
        if [[ $mesh_line =~ IJK=([0-9]+),([0-9]+),([0-9]+) ]]; then
            IBAR="${BASH_REMATCH[1]}"
            JBAR="${BASH_REMATCH[2]}"
            KBAR="${BASH_REMATCH[3]}"
        fi
        
        # --- Extract I_UPPER, J_UPPER from &MULT ---
        mult_line=$(grep -i '&MULT' "$namelist_file")
        
        # Extract I_UPPER and J_UPPER
        if [[ $mult_line =~ I_UPPER=([0-9]+) ]]; then
            I_UPPER="${BASH_REMATCH[1]}"
        fi
        if [[ $mult_line =~ J_UPPER=([0-9]+) ]]; then
            J_UPPER="${BASH_REMATCH[1]}"
        fi

        log "Grid variables from namelist:"
        log " IBAR=$IBAR  JBAR=$JBAR  KBAR=$KBAR  I_UPPER=$I_UPPER  J_UPPER=$J_UPPER"
    fi
    # --------------------------------------------------------------------------    
    
    
       
  
post_exec="$workdir/program.exe"

    # Choose source based on T_END threshold
    if [ "${T_END:-0}" -gt 2000 ]; then
        glue_src="${SCRIPT_DIR%/}/glue_output_partial.f90"
        log "T_END=$T_END > 2000 → using partial postprocessor: $(basename "$glue_src")"
    else
        glue_src="${SCRIPT_DIR%/}/glue_output.f90"
        log "T_END=$T_END ≤ 2000 → using full postprocessor: $(basename "$glue_src")"
    fi

   # Check source + compiler exist
if [ -f "$glue_src" ] && command -v "$COMPILER_CMD" >/dev/null 2>&1; then
    # Recompile only if source is newer than binary or binary absent
    if [ ! -f "$post_exec" ] || [ "$glue_src" -nt "$post_exec" ]; then
        log "Compiling postprocessing program from $glue_src"
        # Attempt to compile; capture compile output in log (already tee'd)
        if ! "$COMPILER_CMD" -mcmodel=medium -o "$post_exec" "$glue_src" \
            -I/usr/local/netcdf-ifort/4.6.1/include -L/usr/local/netcdf-ifort/4.6.1/lib -lnetcdff -shared-intel -lnetcdf -lm; then
            err "Compilation of postprocessing program failed."
            # Don't fatal here if postprocessing optional; continue but warn
            log "Skipping postprocessing compile; continuing without running postprocessor."
            post_exec=""
        else
            chmod +x "$post_exec"
            log "Postprocessing program compiled to $post_exec"
        fi
    else
        log "Postprocessing program up-to-date: $post_exec"
    fi
else
    if [ ! -f "$glue_src" ]; then
        log "Postprocessing source not found: $glue_src (skipping compile/run)"
    else
        log "Fortran compiler '$COMPILER_CMD' not available; skipping compile/run of postprocessor"
    fi
    post_exec=""
fi

# Run postprocessing if available
if [ -n "$post_exec" ] && [ -x "$post_exec" ]; then
    log "Running postprocessing: $post_exec $runname $T_END"
    
    if (( ${T_END:-0} > 2000 )); then
         T_START=2900 
         T_END=2910
         if ! "$post_exec" "$runname" "${T_START}" "${T_END}"  "$IBAR" "$JBAR" "$KBAR" "$I_UPPER" "$J_UPPER"; then
             err "Postprocessing execution failed"
         else
             # Move result to scripts dir if present and out file exists
             out_file="OUT_${runname}.nc"
             if [ -f "$out_file" ]; then
                 mv -f "$out_file" "${SCRIPT_DIR%/}/" || log "Warning: failed to move $out_file to $SCRIPT_DIR"
                 log "Moved $out_file to $SCRIPT_DIR"
             else
                 log "Postprocessor did not produce expected $out_file"
             fi
         fi    
    else
         if ! "$post_exec" "$runname" "${T_END:-0}" "$IBAR" "$JBAR" "$KBAR" "$I_UPPER" "$J_UPPER"; then
             err "Postprocessing execution failed"
         else
             # Move result to scripts dir if present and out file exists
             out_file="OUT_${runname}.nc"
             if [ -f "$out_file" ]; then
                 mv -f "$out_file" "${SCRIPT_DIR%/}/" || log "Warning: failed to move $out_file to $SCRIPT_DIR"
                 log "Moved $out_file to $SCRIPT_DIR"
             else
                 log "Postprocessor did not produce expected $out_file"
             fi
         fi
    fi     
fi

else
    log "Postprocessing disabled by user (-p no)"
fi


# Final runtime report
ending_time=$(date +%s)
elapsed=$(( ending_time - starting_time ))
hours=$(( elapsed / 3600 ))
minutes=$(( (elapsed % 3600) / 60 ))
seconds=$(( elapsed % 60 ))

echo "--------------------------------------------------"
echo "[$(date '+%Y-%m-%d %H:%M:%S')] Run: $runname | Mode: $mode"
echo "ICFILE=${icfile:-none} | BCFILE=${bcfile:-none}"
echo "Total runtime: ${hours}h ${minutes}m ${seconds}s"
echo "Log saved to: $logfile"
echo "Workdir: $workdir"
echo "Postprocessing: $postprocess"
echo "--------------------------------------------------"

# Exit normally (trap will cd back)
exit 0
