cmake_minimum_required(VERSION 2.8)

project(linenoise C)

find_package(Chicken REQUIRED)

chicken_define_extension(${PROJECT_NAME}
    VERSION 0.7
    DESCRIPTION "A minimal, zero-config, BSD licensed, readline replacement."
    CATEGORY "io"
    LICENSE "BSD"
    AUTHOR "Salvatore Sanfilippo"
    MAINTAINER "Christian Kellermann")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0)

chicken_add_module(linenoise linenoise.scm EXTENSION)

chicken_install_modules(${PROJECT_NAME})
