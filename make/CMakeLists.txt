cmake_minimum_required(VERSION 2.8)

project(make C)

find_package(Chicken REQUIRED)

chicken_define_extension(make
    VERSION 1.7
    DESCRIPTION "The PLT 'make' macro"
    CATEGORY "lang-exts"
    LICENSE "LGPL-1"
    AUTHOR "PLT, adapted to Chicken by Felix Winkelmann")

chicken_add_module(make make.scm EXTENSION
    OPTIONS -optimize-level 2 -debug-level 1)

chicken_install_modules(make)
