cmake_minimum_required(VERSION 2.8)

project(coops C)

find_package(Chicken REQUIRED)
chicken_find_extension(matchable REQUIRED)
chicken_find_extension(record-variants REQUIRED)

chicken_define_extension(coops
    VERSION 1.93
    CATEGORY "oop"
    DESCRIPTION "A featureful object system"
    LICENSE "BSD"
    AUTHOR "Dorai Sitaram and Felix Winkelmann")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0 CACHE INTERNAL "")

chicken_add_module(coops coops-module.scm EXTENSION)
chicken_add_module(coops-primitive-objects coops-primitive-objects.scm EXTENSION
    DEPENDS coops)

chicken_install_modules(coops)
