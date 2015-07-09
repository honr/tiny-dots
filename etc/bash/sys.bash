# -*- Mode: Shell-Script -*-

SYSOSTYPE="$(uname)"
case "${SYSOSTYPE}" in
  Darwin)
	  SYSOSTYPE=mac
	  ;;
  Linux)
	  SYSOSTYPE=linux
	  ;;
esac

CPUARCH="$(uname -m)"
case "${CPUARCH}" in
  i*86)
	  CPUARCH=x86
	  ;;
esac
SYSARCH="${SYSOSTYPE}-${CPUARCH}"
