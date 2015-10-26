cmake_minimum_required(VERSION 2.8)

project(regex C)

find_package(Chicken REQUIRED)

chicken_define_extension(regex
    VERSION 1.0
    DESCRIPTION "Compatibility library for old regular expression API"
    CATEGORY "misc"
    LICENSE "BSD")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0)

chicken_add_module(regex regex.scm EXTENSION)

chicken_install_modules(regex)
