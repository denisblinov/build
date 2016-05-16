
rootDir=$(PWD)

# for SGI ALTIX 4700
ifeq (ia64,$(CPU))
  NETCDF_DIR = /RHM-GPFS/software/local/netcdf4
  LIB1=/RHM-GPFS/users/cosmo/anedachina/LibRemDB/LibServHMC.a
  ##LIB1=/RHM-GPFS/data/ASOIHMC/asoihmc/Programm_Alla/LibRemDB/Lib/LibServHMC_Itanium.a
  LIB2=/RHM-GPFS/data/ASOIHMC/dbexp/DataBase/FIELD/bankload.a
  LIB3=$(NETCDF_DIR)/lib/libnetcdf.a
  LIB4=/RHM-GPFS/users/cosmo/dblinov/software/package/bufrdc/bufrdc_000389/ia64/libbufr.a
  PRAGMA=-DNETCDF -DBUFR
endif


# for RSC TORNADO
ifeq (x86_64,$(CPU))
    NETCDF_DIR = /RHM-GPFS/software/ice/local/netcdf4
    ##  LIB1=/RHM-GPFS/users/cosmo/dblinov/software/lib/x86_64/LibServHMC_Xeon.a
    LIB1=/RHM-GPFS/data/ASOIHMC/asoihmc/Programm_Alla/LibRemDB/Lib/LibServHMC_Xeon.a
    #LIB2=/RHM-GPFS/data/ASOIHMC/dbexp/DataBase/FIELD/bankloadInfSer.a
    LIB2=/RHM-GPFS/data/ASOIHMC/dbexp/DataBase/FIELD/bankload.a
    LIB3=$(NETCDF_DIR)/lib/libnetcdf.a
    LIB4=/RHM-GPFS/users/cosmo/dblinov/software/package/bufrdc/bufrdc_000389/x86_64/libbufr.a
    PRAGMA=-DNETCDF -DBUFR
endif

MYLIB = /RHM-GPFS/users/cosmo/dblinov/software/include/fortran

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
FC = ifort
LD = ifort -static
# FC = gfortran
# LD = gfortran -static

ifeq (opt,$(mode))
    COMFLG1 = -O2 -fpp $(PRAGMA)
    # COMFLG1 = -O2 -fast -parallel -fpp $(PRAGMA)
    # COMFLG1 = -O2 -fast -parallel -fpp $(PRAGMA) -profile-functions -profile-loops
else
    COMFLG1 = -g -O0 -traceback -fpp $(PRAGMA)
    # COMFLG1 = -g -O0 -fbacktrace -cpp $(PRAGMA)    
endif

#------------------------------------------------------------------------------

INCL = -I$(NETCDF_DIR)/include -I$(MYLIB) -I.
#------------------------------------------------------------------------------

PROGRAM = remdb2data

remdb2data_$(CPU):  module_main.f90                          \
                    module_bufr.o module_netCDF.o
		echo ' making remdb_$(CPU).exe'
		$(FC) $(COMFLG1) -c module_main.f90
		echo ' compile mod_main'
		$(FC) $(COMFLG1) $(INCL) -c module_bufr.f90
		echo ' compile mod_bufr'
		$(FC) $(COMFLG1) $(INCL) -o  remDB_$(CPU)  remDB.f90 \
		$(MYLIB)/convertCoord.f90 $(MYLIB)/handle_error.f90 \
		$(MYLIB)/stats_array.f90 \
		module_main.o module_netCDF.o module_bufr.o \
		$(LIB1) $(LIB2) $(LIB3) $(LIB4)
		echo ' linking remdb_$(CPU).exe'
module_bufr.o:    module_bufr.f90
		$(FC) $(COMFLG1) $(INCL) -c module_bufr.f90
		echo ' compile mod_bufr'
module_netCDF.o:  module_netCDF.f90
		echo 'proc=' $(CPU)
		$(FC) $(INCL) $(COMFLG1) -c module_netCDF.f90
		echo ' compile mod_netcdf'
TEST:
		cd $(rootDir)/test
		$(rootDir)/remDB_$(CPU)* >log
AMS:    module_main.f90 module_bufr.o  module_netCDF.o
		$(FC) $(COMFLG1) -c module_main.f90
		echo ' compile mod_main'
		$(FC) $(COMFLG1) $(INCL) -o  ams  ReadTAMS.f90 \
		module_main.o module_netCDF.o module_bufr.o \
		$(LIB1) $(LIB2) $(LIB3) $(LIB4)
		echo ' linking ReadTAMS.exe'

# ---------------- Cleaning: ------------------------------
.PHONY: clean
clean :
		@echo cleaning
		-rm  *.o *.mod remDB_* test/cdfin_*
#-=--------------------------------------------------------



