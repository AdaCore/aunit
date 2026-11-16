BUILD_FLAGS = -f
CUSTOM_CLEAN = 1

include ../common.mk

coverage:
	$(GPRBUILD) $(GPROPTS) -p $(BUILD_FLAGS) -P$(PROJECT) -XCOVERAGE=yes
	./test_calculator
	cd obj;	gcov ../../tested_lib/obj/*.gcda

clean:
	gprclean -Pharness
	gprclean -Ptested_lib/testlib
	-rm -rf obj
	-rm -rf tested_lib/obj
	-rm -rf tested_lib/lib
