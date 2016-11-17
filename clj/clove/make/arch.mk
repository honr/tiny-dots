# defines UNAME,
# defines and creates $(BUILDDIR) $(DOBJ), $(DBIN), $(DLIB) directories.

UNAME := $(shell uname)

LIBEXT := so
ifeq ($(UNAME), Linux)
UNAME=linux
JAVADIR := /usr/lib/jvm/java-9-openjdk-amd64

# LIB_PTHREAD=-lpthread
# LIB_HIST=-lhistory -ltermcap
LIBSWITCH=-shared
LIBEXT := so
INCLUDESJAVA := -Ibuild/include -I/usr/include
INCLUDESJAVA += -I/$(JAVADIR)/include
INCLUDESJAVA += -I/$(JAVADIR)/include/linux

else ifeq ($(UNAME), Darwin)
UNAME=darwin
JDKVERSION := $(shell basename /Library/Java/JavaVirtualMachines/*)
JAVADIR := /Library/Java/JavaVirtualMachines/$(JDKVERSION)/Contents/Home
LIBSWITCH=-dynamiclib
LIBEXT := dylib
INCLUDESJAVA := -Ibuild/include -I/usr/include -I/System/Library/Frameworks/JavaVM.framework/Headers -I/Library/Java/Home/include
INCLUDESJAVA += -I/$(JAVADIR)/include
INCLUDESJAVA += -I/$(JAVADIR)/include/darwin
endif

BUILDDIR := build
DOBJ := $(BUILDDIR)/obj/$(UNAME)
DBIN := $(BUILDDIR)/bin/$(UNAME)
DLIB := $(BUILDDIR)/lib/$(UNAME)

dirs: $(BUILDDIR) $(DOBJ) $(DBIN) $(DLIB)

$(BUILDDIR):
	mkdir -p $@
$(DOBJ):
	mkdir -p $@
$(DBIN):
	mkdir -p $@
$(DLIB):
	mkdir -p $@
