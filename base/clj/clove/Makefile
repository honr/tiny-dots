# Example run:
# make JAVADIR=/usr/lib/jvm/java-9-openjdk-amd64

# CFLAGS := -Wall -O3
CFLAGS := -g -Wall -Wextra -std=gnu99 -pedantic -O3
# CFLAGS := -g -Wall -pg -O3

default: all
include make/arch.mk
SRC = src/clove
INSTALLDIR := $(HOME)/.local
CONFIGDIR := $(HOME)/.config

TARGETs = dirs
TARGETs += $(DBIN)/clove
TARGETs += $(DBIN)/clove-daemon
TARGETs += $(DLIB)/libCloveNet.$(LIBEXT)
TARGETs += $(BUILDDIR)/clove.jar

all: $(TARGETs)

$(DOBJ)/clove-common.o: $(SRC)/clove-common.c $(SRC)/clove-common.h
	gcc $(CFLAGS) -fPIC -c -o $@ $<

$(DOBJ)/strl.o: $(SRC)/strl.c $(SRC)/strl.h
	gcc $(CFLAGS) -fPIC -c -o $@ $<

$(DOBJ)/clove-utils.o: $(SRC)/clove-utils.c $(SRC)/clove-utils.h $(SRC)/clove-common.h $(SRC)/strl.h
	gcc $(CFLAGS) -fPIC -c -o $@ $<

$(DOBJ)/clove-daemon.o: $(SRC)/clove-daemon.c $(SRC)/clove-utils.h $(SRC)/clove-common.h $(SRC)/strl.h
	gcc $(CFLAGS) -fPIC -c -o $@ $<

$(DOBJ)/clove.o: $(SRC)/clove.c $(SRC)/clove-utils.h $(SRC)/clove-common.h $(SRC)/strl.h
	gcc $(CFLAGS) -fPIC -c -o $@ $<

$(DBIN)/clove-daemon: $(DOBJ)/clove-daemon.o $(DOBJ)/clove-utils.o $(DOBJ)/strl.o $(DOBJ)/clove-common.o
	gcc $(CFLAGS) $^ -o $@

$(DBIN)/clove: $(DOBJ)/clove.o $(DOBJ)/clove-utils.o $(DOBJ)/strl.o $(DOBJ)/clove-common.o
	gcc $(CFLAGS) $^ -lreadline -o $@ # -lhistory -ltermcap

$(BUILDDIR)/include/clove_CloveNet.h: $(SRC)/*.java
	javac -d build -g:none $(SRC)/*.java
# TODO: ^^ find the option to includeJavaRuntime
	javah -d $(BUILDDIR)/include -classpath build clove.CloveNet

$(DLIB)/libCloveNet.$(LIBEXT): $(BUILDDIR)/include/clove_CloveNet.h $(DOBJ)/clove-common.o
	gcc -Wall -fPIC -c $(INCLUDESJAVA) $(SRC)/CloveNet.c -o $(DOBJ)/CloveNet.o
	gcc -o $(DLIB)/libCloveNet.$(LIBEXT) $(LIBSWITCH) $(DOBJ)/CloveNet.o $(DOBJ)/clove-common.o

build/clove.jar: $(BUILDDIR)/include/clove_CloveNet.h
	cd build ; jar cf clove.jar clove/*.class
#	jar cf clove.jar -C build clove/*.class
#	stupid jar! Could have an option to strip directory ("build" here)

install: all
	install -d $(INSTALLDIR)/bin/$(UNAME)
	install -d $(INSTALLDIR)/lib/$(UNAME)
	install -d $(INSTALLDIR)/share/java/lib-local
	install $(DBIN)/clove $(INSTALLDIR)/bin/$(UNAME)
	install $(DBIN)/clove-daemon $(INSTALLDIR)/bin/$(UNAME)
	install $(DLIB)/libCloveNet.$(LIBEXT) $(INSTALLDIR)/lib/$(UNAME)
	install $(BUILDDIR)/clove.jar $(INSTALLDIR)/share/java/lib-local

	install -d $(INSTALLDIR)/share/clove
	install share/clove/clove.conf $(INSTALLDIR)/share/clove
	install share/clove/clove-clojure $(INSTALLDIR)/share/clove
	install share/clove/clove-clojure.conf $(INSTALLDIR)/share/clove

	install -d $(CONFIGDIR)/autostart
	install clove-daemon.desktop $(CONFIGDIR)/autostart/clove-daemon.desktop

clean:
	-rm -Rf $(BUILDDIR)
