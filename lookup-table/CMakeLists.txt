cmake_minimum_required(VERSION 2.8)

project(lookup-table C)

find_package(Chicken REQUIRED)
chicken_find_extension(setup-helper 1.5.2 REQUIRED)
chicken_find_extension(check-errors 1.10.0 REQUIRED)
chicken_find_extension(miscmacros 2.91 REQUIRED)
chicken_find_extension(record-variants 0.5 REQUIRED)
chicken_find_extension(synch 2.1.0 REQUIRED)

chicken_define_extension(lookup-table
    VERSION 1.13.5
    DESCRIPTION "Simple Lookup Table"
    CATEGORY "data"
    LICENSE "BSD"
    AUTHOR "Kon Lovett")

# MAGIC-LIMIT - Element count when hash-table faster (YMMV)
set(CHICKEN_OPTIONS
    -scrutinize
    -prelude "(define-constant MAGIC-LIMIT 12)"
    -disable-interrupts
    -fixnum-arithmetic
    -local
    -inline-limit 50
    -no-procedure-checks
    -optimize-level 3)

chicken_add_module(lookup-table lookup-table.scm EXTENSION
    OPTIONS -debug-level 1)
chicken_add_module(lookup-table-unsafe lookup-table-unsafe.scm EXTENSION
    OPTIONS -debug-level 0 -feature unsafe -no-bound-checks -no-argc-checks)
chicken_add_module(lookup-table-synch lookup-table-synch.scm EXTENSION
    OPTIONS -debug-level 1
    DEPENDS lookup-table)
chicken_add_module(lookup-table-unsafe-synch lookup-table-unsafe-synch.scm EXTENSION
    OPTIONS -debug-level 0 -feature unsafe -no-bound-checks -no-argc-checks
    DEPENDS lookup-table-unsafe)

chicken_install(EXTENSION lookup-table MODULES)
