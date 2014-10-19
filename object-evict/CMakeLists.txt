cmake_minimum_required(VERSION 2.8)

project(object-evict C)

find_package(Chicken REQUIRED)
find_chicken_extension(srfi-69 REQUIRED)

define_chicken_extension(${PROJECT_NAME}
    CATEGORY "data"
    DESCRIPTION "Evict data to unmanaged memory"
    LICENSE "BSD"
    AUTHOR "The CHICKEN Team")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0)

add_chicken_module(${PROJECT_NAME} ${PROJECT_NAME}.scm EXTENSION)

install_chicken_modules(${PROJECT_NAME})
