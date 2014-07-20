cmake_minimum_required(VERSION 2.8)

project(linenoise C)

find_package(Chicken REQUIRED)

define_chicken_extension(${PROJECT_NAME}
    VERSION 0.7
    DESCRIPTION "A minimal, zero-config, BSD licensed, readline replacement."
    CATEGORY "io"
    LICENSE "BSD"
    AUTHOR "Salvatore Sanfilippo"
    MAINTAINER "Christian Kellermann")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0)

add_chicken_module(linenoise linenoise.scm EXTENSION)

install_chicken_modules(${PROJECT_NAME})
