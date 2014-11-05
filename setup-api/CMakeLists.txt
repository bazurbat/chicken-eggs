cmake_minimum_required(VERSION 2.8)

project(setup-api C)

find_package(Chicken REQUIRED)

define_chicken_extension(${PROJECT_NAME}
    CATEGORY "misc"
    DESCRIPTION "Build and installation API for eggs"
    LICENSE "BSD"
    AUTHOR "The CHICKEN Team")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0)

add_chicken_module(${PROJECT_NAME} ${PROJECT_NAME}.scm EXTENSION)

install_chicken_modules(${PROJECT_NAME})
