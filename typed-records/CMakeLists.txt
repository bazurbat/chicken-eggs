cmake_minimum_required(VERSION 2.8)

project(typed-records C)

find_package(Chicken REQUIRED)
chicken_find_extension(defstruct REQUIRED)
chicken_find_extension(type-stubs REQUIRED)

chicken_define_extension(${PROJECT_NAME}
    VERSION 0.6
    DESCRIPTION "Typed variants of various record-definition macros"
    CATEGORY "misc"
    LICENSE "BSD"
    AUTHOR "Felix Winkelmann")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0 CACHE INTERNAL "")

chicken_add_module(typed-records typed-records.scm EXTENSION)

chicken_install_modules(typed-records)
