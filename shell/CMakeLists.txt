cmake_minimum_required(VERSION 2.8)

project(shell C)

find_package(Chicken REQUIRED)

chicken_define_extension(${PROJECT_NAME}
    VERSION 0.3
    DESCRIPTION "Convenient shell-command invocation"
    CATEGORY "misc"
    LICENSE "Public Domain"
    AUTHOR "Felix Winkelmann")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0 CACHE INTERNAL "")

chicken_add_module(shell shell.scm EXTENSION)

chicken_install(EXTENSION shell MODULES)
