
rootDir=$(PWD)

# for SGI ALTIX 4700
ifeq (ia64,$(CPU))
  NETCDF_DIR = /RHM/software/local/netcdf4
  LIB1=/RHM/RHM-M/users/cosmo/anedachina/LibRemDB/LibServHMC.a
##LIB1=/RHM/RHM-M/data/ASOIHMC/asoihmc/Programm_Alla/LibRemDB/Lib/LibServHMC_Itanium.a
  LIB2=/RHM/RHM-M/data/ASOIHMC/dbexp/DataBase/FIELD/bankload.a
  LIB3=$(NETCDF_DIR)/lib/libnetcdf.a
  LIB4=/RHM/RHM-M/users/cosmo/dblinov/software/package/bufrdc/bufrdc_000389/ia64/libbufr.a
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

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
FC=ifort
LD=ifort -static

#COMFLG1=-O2 -fast -parallel -fpp $(PRAGMA)
COMFLG1=-O0 -fpp  $(PRAGMA)
#COMFLG1= -g -O0 -fpp $(PRAGMA)
#------------------------------------------------------------------------------

 INCL=-I$(NETCDF_DIR)/include -I.
#------------------------------------------------------------------------------

PROGRAM=remDB_$(CPU)

remDB_ia64:  module_main.f90 module_bufr.o  module_netCDF.o
		echo ' making remdb_ia64.exe'
		$(FC) $(COMFLG1) -c module_main.f90
		echo ' compile mod_main'
		$(FC) $(COMFLG1) $(INCL) -c module_bufr.f90
		echo ' compile mod_bufr'
		$(FC) $(COMFLG1) $(INCL) -o  remDB_$(CPU)  remDB.f90 \
		module_main.o module_netCDF.o module_bufr.o \
		$(LIB1) $(LIB2) $(LIB3) $(LIB4)
		echo ' linking remdb_ia64.exe'
remDB_x86_64:  module_main.f90 module_bufr.o module_netCDF.o
		echo ' making remdb_x86_64.exe'
		$(FC) $(COMFLG1) -c module_main.f90
		echo ' compile mod_main'
		$(FC) $(COMFLG1) $(INCL) -c module_bufr.f90
		echo ' compile mod_bufr'
		$(FC) $(COMFLG1) $(INCL) -o  remDB_$(CPU)  remDB.f90 \
		module_main.o module_netCDF.o module_bufr.o \
		$(LIB1) $(LIB2) $(LIB3) $(LIB4)
		echo ' linking remdb_x86_64.exe'
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
		rm  *.o *.mod remDB_* test/cdfin_*
#-=--------------------------------------------------------



