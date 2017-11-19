## Copyright (C) 2016 Artyom V. Poptsov <poptsov.artyom@gmail.com>
##
## This file is part of Guile-ICS.
##
## Guile-ICS is free software: you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## Guile-ICS is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Guile-ICS.  If not, see <http://www.gnu.org/licenses/>.

include $(top_srcdir)/am/guilec

SUBDIRS =

SOURCES = \
	binary.scm	\
	boolean.scm	\
	cal-address.scm \
	date.scm	\
	date-time.scm	\
	duration.scm	\
	float.scm	\
	integer.scm	\
	period.scm	\
	property.scm	\
	recur.scm	\
	text.scm	\
	time.scm

GOBJECTS = $(SOURCES:%.scm=%.go)
moddir=$(guilesitedir)/ics
nobase_dist_mod_DATA = $(SOURCES)

ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/ics/
nobase_dist_ccache_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_ssh_install_go_files = install-nobase_dist_ccacheDATA
$(guile_ssh_install_go_files): install-nobase_dist_modDATA

guilec_warnings =				\
  -Wunbound-variable -Warity-mismatch		\
  -Wunused-variable -Wunused-toplevel

guilec_opts = 					\
	--load-path=$(abs_srcdir)	\
	--load-path=$(abs_builddir)	\
	--target=$(host)			\
	$(guilec_warnings)

# TODO: Move environment setup to a separate file.
guilec_env  = 									\
	GUILE_AUTO_COMPILE=0 							\
	GUILE_LOAD_PATH="$(abs_top_srcdir)/modules"

.scm.go:
	$(AM_V_GUILEC)$(guilec_env) $(GUILEC) $(guilec_opts) \
	--output=$@ $<

CLEANFILES = $(GOBJECTS)

EXTRA_DIST =

clean-go:
	@cd ics; make clean-go
.PHONY: clean-go

## Makefile.am ends here.
