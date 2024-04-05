correlacao.o : $(SRC_PATH)/correlacao.f90 utilsMod.o dump.o \
	memoryMod.o
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

dump.o : $(SRC_PATH)/dump.F90
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

utilsMod.o : $(SRC_PATH)/utilsMod.f90 dump.o
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

filesMod.o : $(SRC_PATH)/filesMod.f90 utilsMod.o dump.o \
	memoryMod.o
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)

memoryMod.o : $(SRC_PATH)/memoryMod.f90 dump.o
	cp -f $< $(<F:.f90=.f90)
	$(F_COMMAND) $(<F:.f90=.f90)
	rm -f $(<F:.f90=.f90)