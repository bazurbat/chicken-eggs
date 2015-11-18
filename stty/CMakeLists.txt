cmake_minimum_required(VERSION 2.8)

project(stty C)

find_package(Chicken REQUIRED)
chicken_find_extension(setup-helper REQUIRED)
chicken_find_extension(foreigners REQUIRED)

chicken_define_extension(${PROJECT_NAME}
    VERSION 0.2.4
    DESCRIPTION "stty-style interface to termios"
    CATEGORY "io"
    LICENSE "BSD"
    AUTHOR "Alex Shinn")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0)

chicken_add_module(stty stty.scm EXTENSION)

chicken_install(EXTENSION ${PROJECT_NAME} MODULES)
