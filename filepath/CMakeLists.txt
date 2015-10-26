cmake_minimum_required(VERSION 2.8)

project(filepath C)

find_package(Chicken REQUIRED)
chicken_find_extension(matchable REQUIRED)

chicken_define_extension(filepath
    VERSION 1.5
    DESCRIPTION "Cross-platform file path manipulation"
    CATEGORY "parsing"
    LICENSE "BSD"
    AUTHOR "Ivan Raikov")

set(CHICKEN_OPTIONS -optimize-level 2 -debug-level 0)

chicken_add_module(filepath filepath.scm EXTENSION)

chicken_install_modules(filepath)
