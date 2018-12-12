.SUFFIXES:.out .o .s .c .F .f .f90 .e .r .y .yr .ye .l .p .sh .csh .h

FCOMP = gfortran
#FCOMP = g77
#FCOMP = ifort
#FCOMP = pgf90

CCOMP = gcc
#CCOMP = icc

#PGFLAGS = -c -Mprof=func -O2
#CFLAGS = -c -O2
#LFLAGS = -Mprof=func

# Optimization level 3
#PGFLAGS = -c -O3 -Wall -Werror -Warray-bounds -Wconversion -Wunderflow -Wsurprising -fbacktrace -fdump-core
#CFLAGS = -c -O3

# Fast Intel flags for C2D machines
#PGFLAGS = -c -fast -m64
#CFLAGS = -c -fast -m64
#LFLAGS = -fast

# Debugging flags
#PGFLAGS = -c -g -debug all -warn all
#PGFLAGS = -c -g3
#CFLAGS = -c -g3
PGFLAGS = -c -g3 -w -fbacktrace -fdump-core
CFLAGS = -c -g3


#optimized for gfortran
#PGFLAGS = -c -m64 -O3
#CFLAGS = -c -m64 -O3


#trial for cluster bjy 111609
#PGFLAGS = -c
#CFLAGS = -c

#trial for ifort, bjy 111609
#PGFLAGS = -c -m64 -fast
#CFLAGS = -c -m64 -fast

# this where the X11 include files must be (system dependent)
# if your system is properly setup, leave it blank  

#INCLUDE = /usr/openwin/include
#INCLUDE = /usr/lib/gcc/x86_64-linux-gnu/4.3/include
#INCLUDE = /usr/lib/gcc/i686-apple-darwin9/4.2.1/include
#FLAGS = -c -O -L/opt/openmpi/include/

TYPE = serial # define here whether you want "serial" or "mpi"

# choice of compiler (must contain mpi if parallel computing)
# # note this second has to be changed by user
# # first one is for parallel, second for serial
#FORT = /cluster/apps/openmpi/1.2.7/x86_64/intel/bin/mpif90 # mpi
#FORT = mpif90
#FORT = ifort # serial

# choose one for parallel or serial
#MPI = -cpp -DICE_MPI=1 # mpi
#MPI = -fpp # serial

#CFLAGS = -c -O -w

OBJECTS = \
cascade_globals.o \
rt_param.o \
debug.o \
check_var.o \
basal_temperature.o \
surface_temperature.o \
time_step.o \
total_topography.o \
update_height.o \
calve.o \
avalanche.o \
write_int.o \
check_for_removal.o \
change_sea_level.o \
find_order.o \
fluvial_erosion.o \
del_sub.o \
delaun.o \
nn2.o \
find_surface.o \
find_neighbours.o \
find_donors.o \
nn_remove.o \
check_mesh.o \
build_a.o \
solve_diffusion.o \
diffusion_erosion.o \
erosional_properties.o \
MOVEtoCascadeVelocityField.o \
tectonic_uplift.o \
find_catchment.o \
four1.o \
realft.o \
sinft.o \
flexure.o \
read_but_skip_comment.o \
iread_but_skip_comment.o \
initialize_nodal_geometry.o \
readmesh.o \
read_nodal_geometry.o \
del_flip.o \
tectonic_movement.o \
update_bedrock.o \
update_time_step.o \
write_tecplot_output.o \
write_output.o \
rainmaker.o \
landslide.o \
landslide_simple.o \
find_dslope.o \
nn1.o \
nnplot.o \
qhullf_dummy.o \
stack.o \
stackpair.o \
volume.o \
mb_ice.o \
interp_ice.o \
export_to_comsol.o \
update_height_comsol.o \
import_from_comsol.o \
ICE.o  \
gerode_node.o \
mass_balance.o \
update_flags.o \
terosion.o \
nrtype.o \
MOVEtoCascade.o \
checkMeshResolution.o \
cascade.o

# needed because the make command thinks .mod files are compiles with a MODULA compiler
%.o : %.mod

.f90.o:
	$(FCOMP) $(PGFLAGS) $*.f90
.c.o:
	$(CCOMP) $(CFLAGS) $*.c

all: icecascade

icecascade:	$(OBJECTS) 
		$(FCOMP)  *.o -o icecascade

# needed dependencies:
#cascade.o:	rt_param.o cascade_globals.o
#rt_param.o:	cascade_globals.o

clean:
	rm -f *.o *.mod *__genmod.f90 *~
