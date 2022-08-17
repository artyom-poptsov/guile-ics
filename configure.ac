dnl   configuration script for Guile-ICS
dnl   Process this file with autoconf to produce configure.
dnl

define(GUILE_ICS_CONFIGURE_COPYRIGHT, [[
Copyright (C) 2017-2022 Artyom V. Poptsov <poptsov.artyom@gmail.com>

This file is part of Guile-ICS.

Guile-ICS is free software: you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

Guile-ICS is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with Guile-ICS.  If not, see <http://www.gnu.org/licenses/>.
]])

AC_INIT([Guile-ICS], [0.2.1], [poptsov.artyom@gmail.com],
                     [guile-ics],
                     [https://github.com/artyom-poptsov/guile-ics])

AC_COPYRIGHT(GUILE_ICS_CONFIGURE_COPYRIGHT)

AC_CONFIG_AUX_DIR([build-aux])
AC_CONFIG_MACRO_DIR([build-aux/m4])

AM_INIT_AUTOMAKE([gnu silent-rules -Wall -Wno-portability
                      color-tests])

m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])],
                            [AC_SUBST([AM_DEFAULT_VERBOSITY],1)])

AC_ARG_WITH([guilesitedir],
            [AS_HELP_STRING([--with-guilesitedir],
                            [use the specified installation path for Guile modules])],
            [case "x$withval" in
             xyes|xno) guilesitedir="";;
             *)        guilesitedir="$withval";;
             esac],
             [guilesitedir=""])


dnl ----------------------------------------------------------------------
dnl Guile-SMC standalone mode.
AC_ARG_ENABLE(
  [standalonemode],
  [AS_HELP_STRING(
    [--enable-standalonemode],
    [Use the standalone mode for the Guile state machine compiler (Guile-SMC)])])

AS_IF(
  [test "x$enable_standalonemode" = "xyes"],
  [AC_MSG_NOTICE([Guile-SMC standalone mode enabled])
  AC_DEFINE(SMC_STANDALONE_MODE, 1, [Guile-SMC standalone mode enabled])],
  [AC_DEFINE(SMC_STANDALONE_MODE, 0, [Guile-SMC standalone mode disabled])]
)

AM_CONDITIONAL([ENABLE_SMC_STANDALONE_MODE], [test "x$enable_standalonemode" = "xyes"])
dnl ----------------------------------------------------------------------


dnl These macros must be provided by guile.m4.
m4_pattern_forbid([^GUILE_PKG$])
m4_pattern_forbid([^GUILE_PROGS$])

GUILE_PKG([3.0 2.0 2.2])

GUILE_PROGS
GUILE_FLAGS
GUILE_SITE_DIR

if test "x$guilesitedir" = "x"; then
   guilesitedir="$datadir/guile/site/$GUILE_EFFECTIVE_VERSION"
fi
AC_SUBST([guilesitedir])

AC_CONFIG_FILES([
    Makefile
    build-aux/Makefile
    build-aux/am/Makefile
    build-aux/m4/Makefile
    examples/Makefile
    modules/Makefile
    modules/ics/fsm/Makefile
    modules/ics/Makefile
    modules/ics/type/Makefile
    modules/ics/type/property/Makefile
    tests/Makefile
    utils/Makefile doc/Makefile
])

dnl Manual pages.
AM_MISSING_PROG([HELP2MAN], [help2man])

AC_CONFIG_FILES([pre-inst-env:build-aux/pre-inst-env.in],
   [chmod +x pre-inst-env])

# Generate a Makefile, based on the results.
AC_OUTPUT

if test "$guilesitedir" != "$GUILE_SITE"; then
   # Guile has a different prefix than this module
   AC_MSG_WARN([]
   [The Guile modules will be installed in ${guilesitedir}.]
   [You should probably re-run `configure' with]
   [`--with-guilesitedir=$GUILE_SITE']
   [Otherwise, you will have to adjust the `GUILE_LOAD_PATH' environment]
   [variable.])
fi

dnl configure.ac ends here.
