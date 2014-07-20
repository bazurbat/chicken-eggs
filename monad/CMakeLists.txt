cmake_minimum_required(VERSION 2.8)

project(monad C)

find_package(Chicken REQUIRED)

define_chicken_extension(${PROJECT_NAME}
    VERSION 3.2
    DESCRIPTION "Monads"
    CATEGORY "data"
    LICENSE "BSD"
    URL "https://github.com/dleslie/monad-egg"
    AUTHOR "Daniel J. Leslie")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0)

add_chicken_module(monad monad.scm EXTENSION)

install_chicken_modules(${PROJECT_NAME})
