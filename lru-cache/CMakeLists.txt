cmake_minimum_required(VERSION 2.8)

project(lru-cache C)

find_package(Chicken REQUIRED)
chicken_find_extension(record-variants REQUIRED)

chicken_define_extension(lru-cache
    VERSION 0.5.3
    DESCRIPTION "LRU cache"
    CATEGORY "data"
    LICENSE "BSD"
    AUTHOR "Jim Ursetto")

set(CHICKEN_OPTIONS -optimize-level 2 -debug-level 0 -local -inline)

chicken_add_module(lru-cache lru-cache.scm EXTENSION)

chicken_install(EXTENSION lru-cache MODULES)
