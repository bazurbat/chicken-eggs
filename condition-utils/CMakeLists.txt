cmake_minimum_required(VERSION 2.8)

project(condition-utils C)

find_package(Chicken REQUIRED)
chicken_find_extension(setup-helper 1.5.2 REQUIRED)
chicken_find_extension(check-errors 1.12.0 REQUIRED)

chicken_define_extension(condition-utils
    VERSION 1.0.2
    CATEGORY "misc"
    DESCRIPTION "SRFI 12 Condition Utilities"
    LICENSE "BSD"
    AUTHOR "Kon Lovett")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0 -fixnum-arithmetic
    -no-procedure-checks)

chicken_add_module(condition-utils condition-utils.scm EXTENSION)

chicken_install_modules(condition-utils)
