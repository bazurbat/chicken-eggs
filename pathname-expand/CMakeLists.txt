cmake_minimum_required(VERSION 2.8)

project(pathname-expand C)

find_package(Chicken REQUIRED)

chicken_define_extension(${PROJECT_NAME}
    VERSION 0.1
    CATEGORY "os"
    DESCRIPTION "Pathname expansion"
    LICENSE "BSD"
    AUTHOR "The CHICKEN team")

chicken_add_module(pathname-expand pathname-expand.scm EXTENSION)

chicken_install(EXTENSION ${PROJECT_NAME} MODULES)
