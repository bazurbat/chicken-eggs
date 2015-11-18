cmake_minimum_required(VERSION 2.8)

project(setup-helper C)

find_package(Chicken REQUIRED)

chicken_define_extension(${PROJECT_NAME}
    VERSION 1.5.3
    DESCRIPTION "Installation/Setup Shortcut Routines"
    CATEGORY "misc"
    LICENSE "BSD"
    AUTHOR "Kon Lovett")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0)

chicken_add_module(setup-helper-mod setup-helper-mod.scm EXTENSION)

chicken_install(EXTENSION setup-helper MODULES
    FILES setup-helper.scm)
