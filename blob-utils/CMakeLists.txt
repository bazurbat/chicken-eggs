cmake_minimum_required(VERSION 2.8)

project(blob-utils C)

find_package(Chicken REQUIRED)
chicken_find_extension(setup-helper 1.5.2 REQUIRED)
chicken_find_extension(string-utils 1.2.2 REQUIRED)

chicken_define_extension(${PROJECT_NAME}
    VERSION 1.0.3
    DESCRIPTION "Blob Utilities"
    CATEGORY "data"
    LICENSE "BSD"
    AUTHOR "Kon Lovett")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0 -no-procedure-checks
    -no-argc-checks -no-bound-checks)

chicken_add_module(blob-set-int blob-set-int.scm EXTENSION)

chicken_add_module(blob-hexadecimal blob-hexadecimal.scm EXTENSION)

chicken_install_modules(blob-utils)
