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

EXTRA_DIST = \
	fosdem.scm.in \
	schedule.scm.in

examplesdir = $(pkgdatadir)/examples
dist_examples_DATA = \
	fosdem.scm \
	schedule.scm

substitute = sed -e 's,[@]GUILE[@],$(GUILE),g'

SUFFIXES = .in

.in:
	$(substitute) "$^" > "$@.tmp" && 	\
	chmod +x "$@.tmp" &&			\
	mv "$@.tmp" "$@"

CLEANFILES = \
	fosdem.scm \
	schedule.scm
