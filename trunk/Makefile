
GFORTRAN=gfortran
FFLAGS=-ffixed-line-length-none
PROG=yangs
VERSION=0.1
SOURCES=yangs_routine.f
OBJS=${SOURCES:.f=.o}

all: yangs

yangs: ${SOURCES}
	${GFORTRAN} ${FFLAGS} -c ${SOURCES}

dist:
	@mkdir ${PROG}-${VERSION}
	@cp ${SOURCES} ${PROG}-${VERSION}
	@cp Makefile ${PROG}-${VERSION}
	@tar cvfz ${PROG}-${VERSION}.tar.gz ${PROG}-${VERSION} 
	@rm -rf ${PROG}-${VERSION}

clean:
	@rm -f ${PROG} ${OBJS} core

