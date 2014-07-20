cmake_minimum_required(VERSION 2.8)

project(check-errors C)

find_package(Chicken REQUIRED)
find_chicken_extension(setup-helper 1.5.2 REQUIRED)

define_chicken_extension(${PROJECT_NAME}
    VERSION 1.13.0
    CATEGORY "misc"
    DESCRIPTION "Argument checks & errors"
    LICENSE "BSD"
    AUTHOR "Kon Lovett")

add_chicken_module(type-errors type-errors.scm EXTENSION
    OPTIONS -fixnum-arithmetic -no-procedure-checks -no-bound-checks)
add_chicken_module(srfi-4-errors srfi-4-errors.scm EXTENSION
    OPTIONS -fixnum-arithmetic -no-procedure-checks -no-bound-checks
    DEPENDS type-errors)
add_chicken_module(type-checks type-checks.scm EXTENSION
    OPTIONS -optimize-level 3 -no-procedure-checks -no-bound-checks
    DEPENDS type-errors)
add_chicken_module(srfi-4-checks srfi-4-checks.scm EXTENSION
    OPTIONS -optimize-level 3 -no-procedure-checks -no-bound-checks
    DEPENDS type-checks srfi-4-errors)
add_chicken_module(check-errors check-errors.scm EXTENSION
    OPTIONS -optimize-level 3 -no-procedure-checks -no-bound-checks
    DEPENDS type-checks type-errors srfi-4-checks srfi-4-errors)

install(FILES inline-type-checks.scm
    DESTINATION ${CHICKEN_DATA_DIR})

install_chicken_modules(check-errors)
