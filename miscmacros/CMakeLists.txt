cmake_minimum_required(VERSION 2.8)

project(miscmacros C)

find_package(Chicken REQUIRED)

chicken_define_extension(miscmacros
    VERSION 2.95
    DESCRIPTION "Various helper macros"
    CATEGORY "lang-exts"
    LICENSE "BSD"
    AUTHOR "Felix Winkelmann")

chicken_add_module(miscmacros miscmacros.scm EXTENSION
    OPTIONS -optimize-level 3 -debug-level 0)

chicken_install(EXTENSION miscmacros MODULES)
