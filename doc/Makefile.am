## Makefile.am -- Config file for GNU Automake.
##
## Copyright (C) 2017 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

info_TEXINFOS = guile-ics.texi

guile_ics_TEXINFOS = \
	guile-ics.texi	\
	fdl.texi

dist_man1_MANS =		\
	ics.1

## Silent rules.
AM_V_HELP2MAN = $(AM_V_HELP2MAN_$(V))
AM_V_HELP2MAN_ = $(AM_V_HELP2MAN_$(AM_DEFAULT_VERBOSITY))
AM_V_HELP2MAN_0 = @echo "  HELP2MAN" $@;
##

gen_man =							\
	LANGUAGE= $(top_builddir)/pre-inst-env $(HELP2MAN)	\
	$(HELP2MANFLAGS)

HELP2MANFLAGS = --source=GNU --info-page=$(PACKAGE_TARNAME)

ics.1: $(abs_top_builddir)/utils/ics
	-$(AM_V_HELP2MAN)$(gen_man) --output="$@" "$<"

CLEANFILES = \
	ics.1

## Makefile.am ends here
