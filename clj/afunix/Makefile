CFLAGS=-Wall
# CFLAGS=-g -Wall -pg

UNAME := $(shell uname)
UNAMEM := $(shell uname -m)

LIBEXT := so
ifeq ($(UNAME), Linux)
PLATFORM=linux
LIB_PTHREAD=-lpthread
LIB_HIST=-lhistory -ltermcap
LIBSWITCH=-shared
LIBEXT := so
# TODO: Fix non amd64.
INCLUDESJAVA := -Ibuild/include -I/usr/include
INCLUDESJAVA += -I/usr/lib/jvm/java-7-openjdk-amd64/include
INCLUDESJAVA += -I/usr/lib/jvm/java-7-openjdk-amd64/include/linux
INCLUDESJAVA += -I/usr/lib/jvm/java-6-openjdk-amd64/include
INCLUDESJAVA += -I/usr/lib/jvm/java-6-openjdk-amd64/include/linux
INCLUDESJAVA += -I/usr/lib/jvm/java-6-openjdk/include
INCLUDESJAVA += -I/usr/lib/jvm/java-6-openjdk/include/linux
else ifeq ($(UNAME), Darwin)
PLATFORM=mac
LIBSWITCH=-dynamiclib
LIBEXT := dylib
INCLUDESJAVA := -Ibuild/include -I/usr/include -I/System//Library/Frameworks/JavaVM.framework/Headers -I/Library/Java/Home/include
endif

ifeq ($(UNAMEM), i686)
UNAMEM := x86
else ifeq ($(UNAMEM), i586)
UNAMEM := x86
else ifeq ($(UNAMEM), i486)
UNAMEM := x86
else ifeq ($(UNAMEM), i386)
UNAMEM := x86
else ifeq ($(UNAMEM), amd64)
UNAMEM := x86_64
endif

ARCHI := $(PLATFORM)-$(UNAMEM)

all: dirs build/afunix.jar

dirs: build build/obj build/lib
build:
	mkdir -p $@
build/obj:
	mkdir -p $@
build/lib:
	mkdir -p $@

build/afunix.jar: src/afunix/*.java
	javac -d build -g:none src/afunix/*.java 
# TODO: ^^ find the option to includeJavaRuntime

	javah -d build/include -classpath build afunix.NativeUnixSocket
	gcc -Wall -fPIC -c $(INCLUDESJAVA) src/afunix/NativeUnixSocket.c -o build/NativeUnixSocket.o
	gcc -o build/lib/libNativeUnixSocket.$(LIBEXT) $(LIBSWITCH) build/NativeUnixSocket.o

	cd build ; jar cf afunix.jar afunix/*.class
#	jar cf afunix.jar -C build afunix/*.class
#	stupid jar! Could have an option to strip directory ("build" here)

INSTALLDIR := $(HOME)/.local

install: all
	install -d $(INSTALLDIR)/lib/$(ARCHI)
	install -d $(INSTALLDIR)/share/java/lib-local
	install build/lib/libNativeUnixSocket.$(LIBEXT) $(INSTALLDIR)/lib/$(ARCHI)
	install build/afunix.jar $(INSTALLDIR)/share/java/lib-local

clean:
	-rm -Rf build
