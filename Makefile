# Makefile for SIOD TR - The Reawakening
# George J. Carrette, gjc@alum.mit.edu
#  Modifications by Scáth
#
#  2025-12-30 	  - Cleanup of a lot of old build targets.
#  		  - The historical Makefile is in the attic directory
#  		
#
# Note: The recursive call to make attempts to set LD_LIBRARY_PATH
#       to include the current directory, which is required for the execution
#       of the SIOD compiler. If you are debugging in the current directory
#       you must also set environment LD_LIBRARY_PATH=.
#       Otherwise SIOD won't work at all, or whats worse, the previously
#       installed libsiod will override the local copy you are trying to debug.
#      
#       For debug builds, use make XXX CDEBUG=-g
#
#       You might also need "make SHELL=/bin/sh target" if your default shell
#       is not compatible with sh or ksh.
#
#       Static executable 'sample' is built for comparison purposes
#       on some platforms.
#
# 
IROOT=/usr/local
MANSEC=1
MANDIR=$(IROOT)/man/man$(MANSEC)
BINDIR=$(IROOT)/bin
INCDIR=$(IROOT)/include
LIBDIR=$(IROOT)/lib
LIBSIODDIR=$(LIBDIR)/siod
CP_F=cp -f
# -Wmissing-prototypes
GCCW=-Wall -Wstrict-prototypes
#
SAMPLE_OBJS = sample.o slib.o sliba.o trace.o
SIOD_OBJS_COMMON = slib.o sliba.o trace.o slibu.o md5.o \
		   siod_json.o siod_readline.o baroque.o 

HS_REGEX_OBJS=regcomp.o regerror.o regexec.o regfree.o

# Raylib graphics support (optional)
RAYLIB_AVAILABLE := $(shell test -f /usr/local/lib/libraylib.so && echo yes || echo no)
ifeq ($(RAYLIB_AVAILABLE),yes)
    RAYLIB_CFLAGS = -I/usr/locla/include -DHAVE_RAYLIB
    RAYLIB_LDFLAGS = -L/usr/local/lib -lraylib -lGL -lm -lpthread -ldl -lrt -lX11
endif

# PLplot plotting support (optional)
PLPLOT_AVAILABLE := $(shell pkg-config --exists plplot && echo yes || echo no)
ifeq ($(PLPLOT_AVAILABLE),yes)
    PLPLOT_CFLAGS := $(shell pkg-config --cflags plplot)
    PLPLOT_LIBS := $(shell pkg-config --libs plplot)
else
    $(warning PlPlot not found)
endif

CJSON_AVAILABLE := $(shell pkg-config --exists libcjson && echo yes || echo no)
ifeq ($(CJSON_AVAILABLE), yes)
    JSON_CFLAGS = `pkg-config --cflags libcjson`
    JSON_LDFLAGS = `pkg-config --libs libcjson`
endif


# Readline support (optional but recommended)
# Check if readline is available
READLINE_AVAILABLE := $(shell pkg-config --exists readline && echo yes || echo no)
ifeq ($(READLINE_AVAILABLE),yes)
    READLINE_CFLAGS = `pkg-config --cflags readline` -DHAVE_READLINE
    READLINE_LDFLAGS = `pkg-config --libs readline`
else
    READLINE_CFLAGS =
    READLINE_LDFLAGS =
endif
#
#
default:
	@echo "*****************************************************"
	@echo "* Please specify target from operating system list: *"
	@echo "*  osf1 hpux solaris linux sgi sco unknown          *"
	@echo "*  gccflags hpuxgcc                                 *"
	@echo "*****************************************************"
	@echo
	@echo  " dist    ... siod-nt.tar.gz"
	@echo  " install ... copies to $(BINDIR) etc, see Makefile for doc"
	@echo  " clean   ... delete objects and binaries."

.SUFFIXES: .o .so .man .txt .sl .dylib .smd

# the build_driver is the target of the recursive call to make.

CMDFILES = csiod snapshot-dir snapshot-compare http-get cp-build \
           ftp-cp ftp-put ftp-test ftp-get http-stress proxy-server

build_driver: $(PROGS) $(EXTRA_PROGS) $(CMDFILES)
	@echo "Build done."

LDLP=LD_LIBRARY_PATH=.:$$LD_LIBRARY_PATH
SLD=-DSIOD_LIB_DEFAULT=\\\"$(LIBSIODDIR)\\\"


# Tested by DDC on an OrangePi  running Linux 6.1.43-rockchip-rk3588
# under Ubuntu 22.04. Additional modules such as ndbm.c
# may work without modification in other environments.
#
# sqllite3 support has been added and tested under the above
#
# uname = Linux

linux:
	$(MAKE) $(LDLP) \
	PROGS="siod tar.so parser_pratt.so ss.so gd.so raylib.so \
	       regex.so acct.so sql_sqlite3.so pthreads.so \
	       plplot.so symengine.so" \
	CC="gcc" \
	LD="gcc" \
	CFLAGS="$(GCCW) $(CDEBUG) -fPIC -O2 -D__USE_MISC -D__USE_GNU -D__USE_SVID -D__USE_XOPEN_EXTENDED -D__USE_XOPEN $(SLD) $(READLINE_CFLAGS)" \
	LD_EXE_FLAGS="-rdynamic -Xlinker -rpath -Xlinker $(LIBDIR) -Xlinker -rpath -Xlinker $(LIBSIODDIR)" \
	LD_EXE_LIBS="-ldl" \
	LD_LIB_FLAGS="-shared -L/usr/local/lib" \
	LD_LIB_LIBS="-lm -lc -ldl -lcrypt -lsqlite3 -lpthread  -lCQRlib -loct  $(JSON_LDFLAGS) $(READLINE_LDFLAGS)" \
	SO="so" \
        build_driver

# macOS / Darwin
# Install dependencies with: brew install libgd sqlite3
# uname = Darwin
darwin:
	$(MAKE) $(LDLP) \
	PROGS="siod tar.dylib parser_pratt.dylib ss.dylib gd.dylib raylib.dylib \
	       regex.dylib  sql_sqlite3.dylib pthreads.dylib" \
	CC="clang" \
	LD="clang" \
	CFLAGS="$(GCCW) $(CDEBUG) -Ddarwin -fPIC -O2 $(SLD)" \
	LD_EXE_FLAGS=" -Xlinker -rpath -Xlinker $(LIBDIR) -Xlinker -rpath -Xlinker $(LIBSIODDIR)" \
	LD_EXE_LIBS="" \
	LD_LIB_FLAGS="-dynamiclib -L/usr/local/lib" \
	LD_LIB_LIBS="-lm -lsqlite3 -lpthread -lCQRlib -loct" \
	SO="dylib" \
        build_driver

macos: darwin

unknown:
	-ln -s ssiod siod
	$(MAKE) \
	PROGS="sample ssiod" \
	CFLAGS="$(CDEBUG) -U__osf__ -Usun -USUN5 -Ulinux" \
	LD_EXE_LIBS="-lm" \
	build_driver


### Finally, the actual compilation and linking commands.

libsiod.$(SO): $(SIOD_OBJS_COMMON)
	@echo LD_LIBRARY_PATH = $$LD_LIBRARY_PATH
	$(LD) -o libsiod.$(SO) $(LD_LIB_FLAGS) $(SIOD_OBJS_COMMON) \
                               $(LD_LIB_LIBS) 

ssiod: siod.o $(SIOD_OBJS_COMMON)
	$(CC) -o ssiod $(LD_EXE_FLAGS) siod.o \
                       $(SIOD_OBJS_COMMON) $(LD_EXE_LIBS)

siod: siod.o libsiod.$(SO) 
	$(CC) -o siod $(LD_EXE_FLAGS) siod.o libsiod.$(SO) $(LD_EXE_LIBS) -lCQRlib -loct

sample: $(SAMPLE_OBJS)
	$(CC) -o sample $(LD_EXE_FLAGS) $(SAMPLE_OBJS) $(LD_EXE_LIBS)

.o.$(SO):
	$(LD) -o $@ $(LD_LIB_FLAGS) $< libsiod.$(SO) $(LD_LIB_LIBS)

.o.dylib:
	$(LD) -o $@ $(LD_LIB_FLAGS) $< libsiod.dylib $(LD_LIB_LIBS)

#.o.so:
#	$(LD) -o $@ $(LD_LIB_FLAGS) $< libsiod.so $(LD_LIB_LIBS)


# Now the build rules for the extensions

tar.$(SO): tar.o libsiod.$(SO)

ss.$(SO): ss.o libsiod.$(SO)

acct.$(SO): acct.o libsiod.$(SO)


# Note: libgd 2.3.3+ from https://libgd.github.io/
gd.o: gd.c
	$(CC) $(CFLAGS) `pkg-config --cflags gdlib` -c gd.c

gd.$(SO): gd.o libsiod.$(SO)
	$(LD) -o gd.$(SO) $(LD_LIB_FLAGS) gd.o libsiod.$(SO) \
	      `pkg-config --libs gdlib` $(LD_LIB_LIBS)

# Raylib graphics library integration
raylib.o: raylib.c
	$(CC) $(CFLAGS) $(RAYLIB_CFLAGS) -c raylib.c

raylib.$(SO): raylib.o libsiod.$(SO)
	$(LD) -o raylib.$(SO) $(LD_LIB_FLAGS) raylib.o libsiod.$(SO) \
	      $(RAYLIB_LDFLAGS) $(LD_LIB_LIBS)

regex.$(SO): regex.o libsiod.$(SO) $(HS_REGEX_OBJS_NEEDED)
	$(LD) -o regex.$(SO) $(LD_LIB_FLAGS) regex.o $(HS_REGEX_OBJS_NEEDED) \
                 libsiod.$(SO) $(LD_LIB_LIBS)

pthreads.$(SO): pthreads.o libsiod.$(SO)
	$(LD) -o pthreads.$(SO) $(LD_LIB_FLAGS) pthreads.o libsiod.$(SO) \
	      -lpthread $(LD_LIB_LIBS)

sql_sqlite3.$(SO): sql_sqlite3.o libsiod.$(SO)
	$(LD) -o sql_sqlite3.$(SO) $(LD_LIB_FLAGS) sql_sqlite3.o libsiod.$(SO) \
	      -lsqlite3 $(LD_LIB_LIBS)

plplot.$(SO): siod_plplot.o  libsiod.$(SO)
	$(LD) -o plplot.$(SO) $(LD_LIB_FLAGS) siod_plplot.o libsiod.$(SO) \
	      -lplplot $(LD_LIB_LIBS)

SYMENGINE_CFLAGS := -I/usr/local/include
SYMENGINE_LIBS := -lsymengine -lstdc++ -lgmp

symengine.o: symengine.c siod.h
	$(CC) $(CFLAGS) -c symengine.c

symengine.$(SO):  symengine.o  libsiod.$(SO)
	$(LD) -o symengine.$(SO) $(LD_LIB_FLAGS) symengine.o libsiod.$(SO) \
	      $(SYMENGINE_LIBS) $(LD_LIB_LIBS)

siod_json.o: siod_json.c siod_json.h siod.h
	 $(CC) $(CFLAGS) $(JSON_CFLAGS) -c siod_json.c

siod_readline.o: siod_readline.c siod_readline.h siod.h
	$(CC) $(CFLAGS) $(READLINE_CFLAGS) -c siod_readline.c

siod_plplot.o: siod_plplot.c siod.h
	$(CC) $(CFLAGS) $(PLPLOT_CFLAGS) -c siod_plplot.c

baroque.o: baroque.c
	$(CC) $(CFLAGS) -c baroque.c


siod.o: siod.c siod.h

sample.o: sample.c siod.h

slib.o:	slib.c siod.h siodp.h

sliba.o:	sliba.c siod.h siodp.h

trace.o:	trace.c siod.h siodp.h

slibu.o:	slibu.c siod.h siodp.h md5.h

ss.o:	ss.c siod.h ss.h

md5.o:	md5.c md5.h

statfs.o: statfs.c siod.h

tar.o: tar.c siod.h

regex.o: regex.c siod.h

acct.o: acct.c siod.h

parser_pratt.o: parser_pratt.c siod.h

MANPAGES = siod snapshot-dir snapshot-compare http-get \
           cp-build ftp-cp csiod ftp-put ftp-test ftp-get \
           http-stress proxy-server

LIBFILES = http-server.scm http-stress.scm http.scm \
           maze-support.scm pratt.scm siod.scm smtp.scm  \
           cgi-echo.scm find-files.scm \
           hello.scm parser_pratt.scm pop3.scm selfdoc.scm \
	   sample.c siod.html piechart.scm cgi.scm ftp.scm \
           sql_sqlite3-utils.scm gd-utils.scm \
	   pthreads-utils.scm help.scm qol.scm \
	   plplot-utils.scm compat.scm symengine-utils.scm

SOLIBFILES=gd tar ss regex acct  parser_pratt \
           statfs sql_sqlite3 raylib plplot symengine


PUBINCS = siod.h

COMMON_SRCS=README.md siod.c siod.h \
            siodm.c siodp.h slib.c sliba.c slibu.c  \
            ss.c ss.h trace.c md5.c md5.h \
            gd.c tar.c regex.c acct.c statfs.c \
            parser_pratt.c sql_sqlite3.c siod_readline.c siod_readline.h raylib.c

REGEX_SRCS=siod_regex.html cclass.h regcomp.c regex2.h regfree.c \
           cname.h regerror.c utils.h engine.c regex.h regexec.c

UNIX_MK=Makefile

NT_MK=libsiod.def parser_pratt.def \
      tar.def ss.def regex.def release.bat

CMDSRCS = $(CMDFILES:=.smd)

SRCFILES= $(COMMON_SRCS) $(UNIX_MK) $(NT_MK) fixcrlf.smd \
          siod-dist.sh $(REGEX_SRCS)

DISTFILES= $(CMDSRCS) $(LIBFILES) $(SRCFILES) $(MANPAGES:=.man) \
           $(MANPAGES:=.txt)

INTO_BINDIR=$(CMDFILES) siod
INTO_LIBDIR=libsiod.so 

install: $(DISTFILES)
	@echo "Note: This does not do a build. Only installs what already"
	@echo "      sits in the directory."
	-mkdir -p $(MANDIR)
	-mkdir -p $(BINDIR)
	-mkdir -p $(LIBDIR)
	-mkdir -p $(INCDIR)
	-mkdir -p $(LIBSIODDIR)
	-for X in $(INTO_BINDIR)  ; do \
	 $(CP_F) $$X $(BINDIR) ;\
	done
	-for X in $(LIBFILES) ; do \
	 $(CP_F) $$X $(LIBSIODDIR) ;\
	done
	-for X in $(SOLIBFILES) ; do \
	  for E in so ; do \
	   $(CP_F) $$X.$$E $(LIBSIODDIR) ;\
	  done ;\
	done
	-for X in $(INTO_LIBDIR) ; do \
	 $(CP_F) $$X $(LIBDIR) ;\
	done
	-for X in $(MANPAGES) ; do \
	 $(CP_F) $$X.man $(MANDIR)/$$X.$(MANSEC) ;\
	done
	-for X in $(PUBINCS) ; do \
	 $(CP_F) $$X $(INCDIR) ;\
	done
	@echo "Install done."

clean:
	-rm -f *.o *.so *.sl  *.dylib *~ $(MANPAGES:.man=.txt) so_locations \
            siod sample siod.tar siod.tar.gz siod.zip selfdoc.txt TAGS \
	    *.db *.gif  $(CMDFILES)

# make manpage txt files for distribution to people who do not have 
# nroff.

.man.txt:
	nroff -man $< | col -bx > $@

siod.tar: $(DISTFILES)
	tar cvf siod.tar $(DISTFILES)

siod.tar.gz: $(DISTFILES)
	tar cvf - $(DISTFILES) | gzip -c -v > siod.tar.gz

dist: siod.tar.gz siod.zip
	@echo "distribution kit created."

siod.zip: $(DISTFILES)
	zip siod.zip $(DISTFILES)

fixcrlf_problems: $(DISTFILES)
	./siod -v01,-m2 fixcrlf.smd :action=write $(DISTFILES)

# another case of makefile trouble. I need the binding of $(SO)
# here.

selfdoc:
	./selfdoc.scm *.so > selfdoc.txt

# 

WIN95BIN=./release/libsiod.dll ./release/libsiod.lib ./release/siod.exe \
         ./release/parser_pratt.dll 

win95bin.zip: $(WIN95BIN)
	zip -rj win95bin.zip $(WIN95BIN)

whatsup:
	@rlog -R -L RCS/*

csiod:	csiod.smd
	./siod -v01,-m2 csiod.smd csiod.smd \
               :o=csiod :i=$(BINDIR)/siod :p=read


snapshot-dir: snapshot-dir.smd

snapshot-compare: snapshot-compare.smd

http-get: http-get.smd

http-stress: http-stress.smd

cp-build: cp-build.smd

ftp-cp: ftp-cp.smd

ftp-put: ftp-put.smd

ftp-test: ftp-test.smd

ftp-get: ftp-get.smd

proxy-server: proxy-server.smd

gccflags:
	@echo "*********************************"
	@echo "*** built-in gcc defines are: ***"
	gcc -E -dM -x c /dev/null
	@echo "*********************************"

# Note: You can use the following default rule in your
#       own makefiles, but it doesn't work here until siod has
#       been installed to its BINDIR
#      
#.smd:
#	csiod $< :o=$@
#
.smd:
	./siod -v01,-m2 csiod.smd :o=$@ :i=$(BINDIR)/siod $<


# Instead of copying additional files to the winsiod folder
# these rules pull these extra files in.
EXTRA_SRC_FOLDER=../archive
EXTRA_SRC_FILES=acct.c cp-build.smd test-fork.scm  \
                 release.bat \
                siod-dist.sh $(MANPAGES:=.man)

$(EXTRA_SRC_FILES):
	cp -v $(EXTRA_SRC_FOLDER)/$@ $@



