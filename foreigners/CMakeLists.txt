cmake_minimum_required(VERSION 2.8)

project(foreigners C)

find_package(Chicken REQUIRED)
chicken_find_extension(matchable REQUIRED)

chicken_define_extension(foreigners
    VERSION 1.4.1
    DESCRIPTION "Foreign helper macros for Chicken 4"
    CATEGORY "lang-exts"
    LICENSE "BSD"
    AUTHOR "Jim Ursetto")

set(CHICKEN_OPTIONS -optimize-level 2 -debug-level 0)

chicken_add_module(foreigners foreigners.scm EXTENSION
    EMIT_IMPORTS foreigners renaming)

chicken_install(EXTENSION foreigners MODULES)
