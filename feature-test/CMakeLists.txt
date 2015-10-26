cmake_minimum_required(VERSION 2.8)

project(feature-test C)

find_package(Chicken REQUIRED)

chicken_define_extension(feature-test
    VERSION 0.1
    DESCRIPTION "Foreign feature testing"
    CATEGORY "lang-exts"
    LICENSE "BSD"
    AUTHOR "Jim Ursetto")

set(CHICKEN_OPTIONS -optimize-level 2 -debug-level 0)

chicken_add_module(feature-test feature-test.scm EXTENSION)

chicken_install_modules(feature-test
    FILES feature-test-syntax.scm)
