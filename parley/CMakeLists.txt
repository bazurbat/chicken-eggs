cmake_minimum_required(VERSION 2.8)

project(parley C)

find_package(Chicken REQUIRED)
find_chicken_extension(srfi-13 REQUIRED)
find_chicken_extension(srfi-14 REQUIRED)
find_chicken_extension(stty REQUIRED)

define_chicken_extension(${PROJECT_NAME}
    VERSION 0.8.1
    DESCRIPTION "A readline replacement written in scheme based on linenoise."
    CATEGORY "io"
    LICENSE "BSD"
    AUTHOR "Christian Kellermann"
    MAINTAINER "Christian Kellermann")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0)

add_chicken_module(parley parley.scm EXTENSION)
add_chicken_module(parley-auto-completion parley-auto-completion.scm EXTENSION)

install_chicken_modules(${PROJECT_NAME})
