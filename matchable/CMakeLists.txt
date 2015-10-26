cmake_minimum_required(VERSION 2.8)

project(matchable C)

find_package(Chicken REQUIRED)

chicken_define_extension(matchable
    VERSION 3.3
    DESCRIPTION "Hygienic MATCH replacement"
    CATEGORY "lang-exts"
    LICENSE "Public Domain"
    AUTHOR "Alex Shinn")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0)

chicken_add_module(matchable matchable.scm EXTENSION)

chicken_install_modules(matchable)
