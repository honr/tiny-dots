CFLAGS=-Wall
# CFLAGS=-g -Wall -pg

UNAME := $(shell uname)

LIBEXT := so
ifeq ($(UNAME), Linux)
UNAME=linux
JAVADIR := /usr/lib/jvm/java-9-openjdk-amd64
LIB_PTHREAD=-lpthread
LIB_HIST=-lhistory -ltermcap
LIBSWITCH=-shared
LIBEXT := so
# TODO: Fix non amd64.
INCLUDESJAVA := -Ibuild/include -I/usr/include
INCLUDESJAVA += -I/$(JAVADIR)/include
INCLUDESJAVA += -I/$(JAVADIR)/include/linux
else ifeq ($(UNAME), Darwin)
UNMAE=darwin
JDKVERSION := $(shell basename /Library/Java/JavaVirtualMachines/*)
JAVADIR := /Library/Java/JavaVirtualMachines/$(JDKVERSION)/Contents/Home
LIBSWITCH=-dynamiclib
LIBEXT := dylib
INCLUDESJAVA := -Ibuild/include -I/usr/include -I/System//Library/Frameworks/JavaVM.framework/Headers -I/Library/Java/Home/include
INCLUDESJAVA += -I/$(JAVADIR)/include
INCLUDESJAVA += -I/$(JAVADIR)/include/mac
endif

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
	install -d $(INSTALLDIR)/lib/$(UNAME)
	install -d $(INSTALLDIR)/share/java/lib-local
	install build/lib/libNativeUnixSocket.$(LIBEXT) $(INSTALLDIR)/lib/$(UNAME)
	install build/afunix.jar $(INSTALLDIR)/share/java/lib-local

clean:
	-rm -Rf build
