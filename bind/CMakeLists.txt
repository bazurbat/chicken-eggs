cmake_minimum_required(VERSION 2.8)

project(bind C)

find_package(Chicken REQUIRED)
chicken_find_extension(silex)
chicken_find_extension(matchable)
chicken_find_extension(coops)
chicken_find_extension(regex)
chicken_find_extension(make)

chicken_define_extension(bind
    VERSION 1.5.2
    CATEGORY "ffi"
    DESCRIPTION "Automatically generate bindings from C/C++ declarations"
    LICENSE "public domain"
    AUTHOR "Felix Winkelmann"
    URL "http://wiki.call-cc.org/eggref/4/bind")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0
    CACHE INTERNAL "")

file(COPY c.l DESTINATION .)

add_custom_command(
    OUTPUT c.l.scm
    COMMAND ${CHICKEN_INTERPRETER} -s ${PROJECT_SOURCE_DIR}/runsilex.scm
    DEPENDS c.l)

chicken_add_module(cplusplus-object cplusplus-object.scm EXTENSION)
chicken_add_module(bind-translator bind-translator.scm EXTENSION
    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/c.l.scm)
chicken_add_module(bind bind.scm EXTENSION
    DEPENDS bind-translator)
chicken_add_executable(chicken-bind chicken-bind.scm
    DEPENDS bind-translator)
target_link_libraries(chicken-bind ${CHICKEN_LIBRARIES})

chicken_install(EXTENSION bind MODULES
    TARGETS chicken-bind)
