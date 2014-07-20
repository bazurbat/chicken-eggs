cmake_minimum_required(VERSION 2.8)

project(easyffi C)

find_package(Chicken REQUIRED)
find_chicken_extension(silex REQUIRED)
find_chicken_extension(matchable REQUIRED)
find_chicken_extension(regex REQUIRED)
find_chicken_extension(make REQUIRED)

define_chicken_extension(easyffi
    VERSION 1.99.7
    DESCRIPTION "Generates bindings from C/C++ declarations"
    CATEGORY "obsolete"
    LICENSE "BSD"
    AUTHOR "Felix Winkelmann")

set(CHICKEN_OPTIONS -optimize-level 2 -debug-level 0)

file(COPY easyffi.l DESTINATION .)

add_custom_command(
    OUTPUT easyffi.l.scm
    COMMAND ${CHICKEN_INTERPRETER} -s ${PROJECT_SOURCE_DIR}/runsilex.scm
    DEPENDS easyffi.l)

add_chicken_module(easyffi-base easyffi-base.scm EXTENSION
    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/easyffi.l.scm)
add_chicken_module(easyffi easyffi.scm EXTENSION
    DEPENDS easyffi-base.import)
add_chicken_executable(chicken-wrap chicken-wrap.scm
    DEPENDS easyffi-base)

install_chicken_modules(easyffi
    PROGRAMS chicken-wrap)
