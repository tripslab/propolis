# src/Systems/propolis/Makefile

NAME = trips-propolis

include ../Makefile-common

install::
	$(MKINSTALLDIRS) $(prefix)/etc/Data/

install-configs:: DrumGUI*.conf
	$(INSTALL_DATA) $^ $(etcdir)
