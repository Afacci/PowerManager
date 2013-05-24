.SUFFIXES: 
.SUFFIXES: .f90 .mod .o .f .i

PROGRAM     = PowerManager
DEST        = ~/bin/
VERSION     = -1.0
OBJDIR      = ../obj/
#----------ifort compiler options-------------------------------------------------
#FC          = ifort
#OPT         = -O3 #-pg 
#DEB         = -check all -traceback -override-limits -fpp #-warn all -C -CV -CB 
#----------gfortran compiler options----------------------------------------------
FC          = gfortran
OPT         = -O3 -mcmodel=large #-pg 
DEB         = -fbacktrace -cpp -fbounds-check
#---------------------------------------------------------------------------------
#VPATH       = ./ 
#              -qmaxmem=8192 
################################################
#  list of the modules to be compiled          #
################################################
MODS = \
../obj/shared.o \
../obj/inputVar.o \
../obj/fileTools.o \
../obj/myArithmetic.o \
../obj/plantVar.o \
../obj/mathTools.o \
../obj/interfaces.o \
../obj/cmdVar.o \
../obj/energy.o \
../obj/economy.o \
../obj/graphTools.o \
../obj/globalResults.o \
../obj/getSetPoint.o \
../obj/strategies.o \
../obj/euristics.o \
../obj/sun.o \

################################################
#  list of the files to be compiled            #
################################################
OBJS = \
../obj/main.o \
../obj/readGeneral.o \
../obj/readTrigen.o \
../obj/abortExecution.o \
../obj/allocateVar.o \
../obj/vCount.o \
../obj/rewUnit.o \
../obj/matrixRead2.o \
../obj/hCount.o \
../obj/readBoiler.o \
../obj/readChiller.o \
../obj/findEntry2.o \
../obj/readKeyword.o \
../obj/warning.o \
../obj/readLoad.o \
../obj/interpolation.o \
../obj/buildPlant.o \
../obj/checkPlant.o \
../obj/commandline.o \
../obj/aiuto.o \
../obj/endExecution.o \
../obj/constraints.o \
../obj/openUnit.o \
../obj/objFunction.o \
../obj/output.o \
../obj/readEnv.o \
../obj/performances.o \
../obj/deallocateVar.o \
../obj/readThStorage.o \
../obj/readPV.o \
../obj/readSC.o \
#../obj/readSteamPlant.o \

################################################
#rule that creates the dependency of objects from modules
$(OBJS): $(MODS) 
$(OBJDIR)%.o : %.f90
	$(FC) $(OPT) $(DEB) -c $< 
modules : $(MODS)
compile : $(OBJS)
rmobj  : 
	rm -rf $(OBJDIR)*.o $(OBJDIR)*.mod
rmmain :
	if [ -f $(PROGRAM) ] ; then rm $(PROGRAM) ; fi
mvobj : 
	@files=$(shell ls *.o 2> /dev/null | wc -l)
	$(shell if [ $files != 0 ] ; then mv *.o $(OBJDIR) ; fi)

mvmod   :
	 @files=$(shell ls *.mod 2> /dev/null | wc -l)
	 $(shell if [ $files != 0 ] ; then mv *.mod $(OBJDIR) ; fi)
link:
	$(FC) $(OPT) $(DEB) -o $(PROGRAM) $(MODS) $(OBJS)
cpex:   
	@cp -v $(PROGRAM) $(DEST)$(PROGRAM)$(VERSION)
	@chmod u+x $(DEST)$(PROGRAM)$(VERSION)
all     : modules compile mvobj link 
remake  : rmobj rmmain all
install : all cpex
