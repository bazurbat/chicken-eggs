cmake_minimum_required(VERSION 2.8)

project(silex C)

find_package(Chicken REQUIRED)

chicken_define_extension(${PROJECT_NAME}
    VERSION 1.4
    DESCRIPTION "An efficient and powerful lexer generator"
    CATEGORY "parsing"
    LICENSE "BSD"
    AUTHOR "Danny Dub&eacute;")

chicken_add_module(silex silex.scm EXTENSION
    OPTIONS -optimize-level 2 -debug-level 1)

chicken_install(EXTENSION silex MODULES)
