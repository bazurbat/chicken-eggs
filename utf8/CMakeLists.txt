cmake_minimum_required(VERSION 2.8)

project(utf8 C)

find_package(Chicken REQUIRED)
chicken_find_extension(iset REQUIRED)
chicken_find_extension(regex REQUIRED)
chicken_find_extension(make REQUIRED)

chicken_define_extension(${PROJECT_NAME}
    VERSION 3.3.9
    DESCRIPTION "Unicode support"
    CATEGORY "parsing"
    LICENSE "BSD"
    AUTHOR "Alex Shinn")

set(CHICKEN_OPTIONS -fixnum-arithmetic -inline -local -optimize-level 3
    -debug-level 0)

chicken_add_module(utf8-lolevel utf8-lolevel.scm EXTENSION)
chicken_add_module(utf8 utf8.scm EXTENSION
    DEPENDS utf8-lolevel)
chicken_add_module(utf8-srfi-14 utf8-srfi-14.scm EXTENSION
    DEPENDS utf8-lolevel)
chicken_add_module(unicode-char-sets unicode-char-sets.scm EXTENSION)
chicken_add_module(utf8-case-map utf8-case-map.scm EXTENSION
    DEPENDS utf8-lolevel utf8-srfi-14)
chicken_add_module(utf8-srfi-13 utf8-srfi-13.scm EXTENSION
    DEPENDS utf8-lolevel utf8-srfi-14 utf8-case-map)

chicken_install_modules(utf8
    FILES case-map-1.dat case-map-2.dat)
