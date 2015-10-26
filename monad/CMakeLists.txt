cmake_minimum_required(VERSION 2.8)

project(monad C)

find_package(Chicken REQUIRED)

chicken_define_extension(${PROJECT_NAME}
    VERSION 3.2
    DESCRIPTION "Monads"
    CATEGORY "data"
    LICENSE "BSD"
    URL "https://github.com/dleslie/monad-egg"
    AUTHOR "Daniel J. Leslie")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0)

chicken_add_module(monad monad.scm EXTENSION)

chicken_install_modules(${PROJECT_NAME})
