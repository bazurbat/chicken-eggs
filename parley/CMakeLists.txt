cmake_minimum_required(VERSION 2.8)

project(parley C)

find_package(Chicken REQUIRED)
chicken_find_extension(stty REQUIRED)

chicken_define_extension(${PROJECT_NAME}
    VERSION 0.8.1
    DESCRIPTION "A readline replacement written in scheme based on linenoise."
    CATEGORY "io"
    LICENSE "BSD"
    AUTHOR "Christian Kellermann"
    MAINTAINER "Christian Kellermann")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0)

chicken_add_module(parley parley.scm EXTENSION)
chicken_add_module(parley-auto-completion parley-auto-completion.scm EXTENSION)

chicken_install_modules(${PROJECT_NAME})
