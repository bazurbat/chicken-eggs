cmake_minimum_required(VERSION 2.8)

project(stty C)

find_package(Chicken REQUIRED)
find_chicken_extension(setup-helper REQUIRED)
find_chicken_extension(foreigners REQUIRED)

define_chicken_extension(${PROJECT_NAME}
    VERSION 0.2.4
    DESCRIPTION "stty-style interface to termios"
    CATEGORY "io"
    LICENSE "BSD"
    AUTHOR "Alex Shinn")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0)

add_chicken_module(stty stty.scm EXTENSION)

install_chicken_modules(${PROJECT_NAME})
