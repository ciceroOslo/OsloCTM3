#! /bin/bash
echo "To set work environment"
# Export the current project number
export PROJECT=nn9188k
# alias for squeue
#alias squeue='squeue -lA ${PROJECT}'
# Export the work directory
export WORK=/cluster/work/users/${USER}
# Export CICERO directory
export CICERO=/cluster/projects/nn9188k/OsloCTM3
#export WORKSPACE=${HOME}/CTM/OsloCTM3
export WORKSPACE=/cluster/projects/nn9188k/srkri/VOCMIP_OsloCTM3/OsloCTM3
 

# Load modules and setup
module purge
echo "Settings for OsloCTM3git"
module load netCDF-Fortran/4.6.0-iimpi-2022a
export CTM3_INPUT=${CICERO}/Indata_CTM3
export CTM3_USR_INPUT=/cluster/projects/nn9188k/srkri/VOCMIP_OsloCTM3/OsloCTM3/CTM3_RESTART/
export CTM3_ROOT=${WORKSPACE}/src
export NETCDF_ROOT=$EBROOTNETCDFMINFORTRAN
