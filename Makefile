all: generated
	gprbuild -p -P ada_libfswatch

# warn if libfswatch/ is not populated
libfswatch:
	@echo "you need to install the libfswatch library in libfswatch/"
	@exit 1

# generate the Ada binding
generated: libfswatch
	(mkdir -p generated; cd libfswatch/include/libfswatch/c/ ; \
		gcc -C -fdump-ada-spec libfswatch.h -D_TIMEZONE_DEFINED; \
	       	mv *.ads ../../../../generated/)

clean:
	gprclean -P ada_libfswatch
	rm -rf generated
