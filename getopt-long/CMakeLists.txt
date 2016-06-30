cmake_minimum_required(VERSION 2.8)

project(getopt-long)

find_package(Chicken REQUIRED)
chicken_find_extension(matchable REQUIRED)

chicken_define_extension(
    VERSION 1.16
    CATEGORY "misc"
    DESCRIPTION "Command line option parsing."
    LICENSE "LGPL-3"
    AUTHOR "Ivan Raikov")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0 -fixnum-arithmetic -local
    -no-trace -no-procedure-checks -no-bound-checks)

chicken_add_module(getopt-long getopt-long.scm EXTENSION)

chicken_install(EXTENSION getopt-long MODULES)
