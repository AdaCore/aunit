all:
	gprbuild -p -f -Pharness/harness

coverage:
	gprbuild -p -f -Pharness/harness -XCOVERAGE=yes
	./test_calculator
	cd harness/obj;	gcov ../../tested_lib/obj/*.gcda

clean:
	gprclean -Pharness/harness
	gprclean -Ptested_lib/testlib
	-rm -rf harness/obj
	-rm -rf tested_lib/obj
	-rm -rf tested_lib/lib
