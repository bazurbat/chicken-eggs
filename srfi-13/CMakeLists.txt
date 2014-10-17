cmake_minimum_required(VERSION 2.8)

project(srfi-13 C)

find_package(Chicken REQUIRED)
find_chicken_extension(srfi-14 REQUIRED)

define_chicken_extension(${PROJECT_NAME}
    CATEGORY "data"
    DESCRIPTION "SRFI-13 string library"
    LICENSE "BSD"
    AUTHOR "Olin Shivers")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0)

add_chicken_module(${PROJECT_NAME} ${PROJECT_NAME}.scm EXTENSION)

install_chicken_modules(${PROJECT_NAME})
