## Makefile.am -- Config file for GNU Automake.
##
## Copyright (C) 2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

include $(top_srcdir)/build-aux/am/guile.am

SOURCES = \
	context.scm	\
	content-line-context.scm	\
	content-line-parser.scm	\
	stream-context.scm	\
	stream-parser.scm \
	content-line-parser.puml \
	stream-parser.puml

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)/ics/fsm/
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/ics/fsm/

SUFFIXES = .puml

GUILE_SMC_CONTEXT_ARGS = \
	--log-driver file \
	--log-opt "file=context.log" \
	-m "(ics fsm context)"

GUILE_SMC_CONTENT_LINE_PARSER_ARGS = \
	--log-driver file \
	--log-opt "file=content-line-parser.log" \
	-L $(abs_top_srcdir)/modules \
	--fsm-name content-line-parser \
	--fsm-module "(ics fsm content-line-parser)" \
	-U "((ics fsm context) (ics fsm content-line-context))" \
	--validate

GUILE_SMC_STREAM_PARSER_ARGS = \
	--log-driver file \
	--log-opt "file=stream-parser.log" \
	-L $(abs_top_srcdir)/modules \
	--fsm-name stream-parser \
	--fsm-module "(ics fsm stream-parser)" \
	-U "((ics fsm context) (ics fsm stream-context))" \
	--validate

if ENABLE_SMC_STANDALONE_MODE
GUILE_SMC_CONTEXT_ARGS += \
	--standalone
GUILE_SMC_CONTENT_LINE_PARSER_ARGS += \
	--target=guile-standalone
GUILE_SMC_STREAM_PARSER_ARGS += \
	--target=guile-standalone
endif

context.scm:
	@echo "  SMC      $@"
	@smc context $(GUILE_SMC_CONTEXT_ARGS) > $@

content-line-parser.scm: context.scm content-line-parser.puml
	@echo "  SMC      $@"
	@smc compile $(GUILE_SMC_CONTENT_LINE_PARSER_ARGS) content-line-parser.puml > $@

stream-parser.scm: context.scm stream-context.scm
	@echo "  SMC      $@"
	@smc compile $(GUILE_SMC_STREAM_PARSER_ARGS) stream-parser.puml > $@

### Makefile.am ends here.
