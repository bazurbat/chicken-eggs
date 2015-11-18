cmake_minimum_required(VERSION 2.8)

project(kvlists C)

find_package(Chicken REQUIRED)

chicken_define_extension(kvlists
    VERSION 1.0.3
    DESCRIPTION "Provides keyword/value list (or key-value list, for short) operations."
    CATEGORY "data"
    LICENSE "MIT"
    AUTHOR "Arto Bendiken"
    URL "git://github.com/klutometis/kvlists.git")

set(CHICKEN_OPTIONS -optimize-level 2 -debug-level 0)

chicken_add_module(kvlists kvlists.scm EXTENSION)

chicken_install(EXTENSION kvlists MODULES)
