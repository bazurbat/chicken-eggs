cmake_minimum_required(VERSION 2.8)

project(check-errors C)

find_package(Chicken REQUIRED)
chicken_find_extension(setup-helper 1.5.2 REQUIRED)

chicken_define_extension(${PROJECT_NAME}
    VERSION 1.13.0
    CATEGORY "misc"
    DESCRIPTION "Argument checks & errors"
    LICENSE "BSD"
    AUTHOR "Kon Lovett")

chicken_add_module(type-errors type-errors.scm EXTENSION
    OPTIONS -fixnum-arithmetic -no-procedure-checks -no-bound-checks)
chicken_add_module(srfi-4-errors srfi-4-errors.scm EXTENSION
    OPTIONS -fixnum-arithmetic -no-procedure-checks -no-bound-checks
    DEPENDS type-errors)
chicken_add_module(type-checks type-checks.scm EXTENSION
    OPTIONS -optimize-level 3 -no-procedure-checks -no-bound-checks
    DEPENDS type-errors)
chicken_add_module(srfi-4-checks srfi-4-checks.scm EXTENSION
    OPTIONS -optimize-level 3 -no-procedure-checks -no-bound-checks
    DEPENDS type-checks srfi-4-errors)
chicken_add_module(check-errors check-errors.scm EXTENSION
    OPTIONS -optimize-level 3 -no-procedure-checks -no-bound-checks
    DEPENDS type-checks type-errors srfi-4-checks srfi-4-errors)

install(FILES inline-type-checks.scm
    DESTINATION ${CHICKEN_DATA_DIR})

chicken_install_modules(check-errors)
