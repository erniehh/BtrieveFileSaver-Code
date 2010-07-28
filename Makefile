# This file is part of BtrieveFileSaver project, http://btrievefilesave.sourceforge.net/

PROG=	BtrFileSaver

all:
	@echo "make (linux|bsd)"


##########################################################################
###                 UNIX build: linux, bsd
##########################################################################

CFLAGS=		-W -Wall -std=c99 -pedantic -Os -fomit-frame-pointer $(COPT)
LINFLAGS=	-ldl $(CFLAGS)

linux:
	$(CC) $(LINFLAGS) BtrieveFileSaverLib.c main.c -s -o $(PROG)

bsd:
	$(CC) $(CFLAGS) BtrieveFileSaverLib.c main.c -s -o $(PROG)

##########################################################################
###            cleanup
##########################################################################
clean:
	rm -rf *.o *.core $(PROG) *.obj $(PROG).1.txt *.dSYM *.tgz
