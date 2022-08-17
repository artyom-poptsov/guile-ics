## Config file for GNU Automake.
##
## Copyright (C) 2016-2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

include $(top_srcdir)/build-aux/am/guilec

SCM_TESTS = \
	content-line-parser-context.scm \
	content-line-parser.scm \
	ics.scm		\
	types.scm

TESTS = ${SCM_TESTS}

TEST_EXTENSIONS = .scm

AM_TESTS_ENVIRONMENT = \
	abs_top_srcdir="$(abs_top_srcdir)"; export abs_top_srcdir; \
	abs_top_builddir="$(abs_top_builddir)"; export abs_top_builddir; \
	ORIGTERM=${TERM}; export ORIGTERM; \
	TERM=xterm; export TERM; \
	GUILE_AUTO_COMPILE=0; export GUILE_AUTO_COMPILE;

# LOG_COMPILER was introduced in Automake 1.12; don't expect "make
# check" or "make distcheck" to work with earlier versions.
SCM_LOG_COMPILER = \
	$(GUILE)

AM_SCM_LOG_FLAGS = \
	-L "$(top_srcdir)" \
	-L "$(top_srcdir)/modules"	\
	-s

EXTRA_DIST = \
	${SCM_TESTS}

ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/ssh
nobase_dist_ccache_DATA = $(GOBJECTS)

guilec_warnings =				\
  -Wunbound-variable -Warity-mismatch		\
  -Wunused-variable -Wunused-toplevel

guilec_opts = 					\
	$(guilec_warnings)

# TODO: Move environment setup to a separate file.
guilec_env  = 									\
	GUILE_AUTO_COMPILE=0 							\
	GUILE_LOAD_PATH="$(abs_top_srcdir)/modules"

.scm.go:
	$(AM_V_GUILEC)$(guilec_env) $(GUILEC) $(guilec_opts) \
	--output=$@ $<

###

CLEANFILES = \
	$(GOBJECTS)
