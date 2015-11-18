cmake_minimum_required(VERSION 2.8)

project(record-variants C)

find_package(Chicken REQUIRED)

chicken_define_extension(record-variants
    VERSION 0.5.1
    DESCRIPTION "Optimized record access"
    CATEGORY "lang-exts"
    LICENSE "BSD"
    AUTHOR "Jim Ursetto")

chicken_add_module(record-variants record-variants.scm EXTENSION
    OPTIONS -optimize-level 2 -debug-level 0)

chicken_install(EXTENSION record-variants MODULES)
