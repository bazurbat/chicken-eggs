cmake_minimum_required(VERSION 2.8)

project(easyffi C)

find_package(Chicken REQUIRED)
chicken_find_extension(silex REQUIRED)
chicken_find_extension(matchable REQUIRED)
chicken_find_extension(regex REQUIRED)
chicken_find_extension(make REQUIRED)

chicken_define_extension(easyffi
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

chicken_add_module(easyffi-base easyffi-base.scm EXTENSION
    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/easyffi.l.scm)
chicken_add_module(easyffi easyffi.scm EXTENSION
    DEPENDS easyffi-base)
chicken_add_executable(chicken-wrap chicken-wrap.scm
    DEPENDS easyffi-base)
target_link_libraries(chicken-wrap ${CHICKEN_LIBRARIES})

chicken_install_modules(easyffi
    TARGETS chicken-wrap)
