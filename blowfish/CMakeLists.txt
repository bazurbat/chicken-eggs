cmake_minimum_required(VERSION 2.8)

project(blowfish C)

find_package(Chicken REQUIRED)

chicken_define_extension(${PROJECT_NAME}
    VERSION 1.1
    CATEGORY "crypt"
    DESCRIPTION "Blowfish"
    LICENSE "LGPL"
    AUTHOR "rivo"
    URL "https://bitbucket.org/rivo/blowfish-egg")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0)

chicken_add_module(blowfish blowfish.scm EXTENSION)

chicken_install(EXTENSION blowfish MODULES)
