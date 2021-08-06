DESTDIR=

all: generated
	gprbuild -p -P ada_libfswatch

# generate the Ada binding
generated:
	(mkdir -p generated; cd generated ; \
		gcc -C -fdump-ada-spec /usr/include/libfswatch/c/libfswatch.h -D_TIMEZONE_DEFINED ; \
        rename 'libfswatch_c_' '' *.ads ; \
        sed -i 's/libfswatch_c_//g' *.ads)

clean:
	gprclean -P ada_libfswatch
	rm -rf generated

install:
	gprinstall -r -p -P ada_libfswatch --prefix=$(DESTDIR)
