cmake_minimum_required(VERSION 2.8)

project(string-utils C)

find_package(Chicken REQUIRED)
chicken_find_extension(setup-helper 1.5.2  REQUIRED)
chicken_find_extension(miscmacros   2.9    REQUIRED)
chicken_find_extension(lookup-table 1.13.1 REQUIRED)
chicken_find_extension(check-errors 1.12.1 REQUIRED)

chicken_define_extension(${PROJECT_NAME}
    VERSION 1.2.4
    DESCRIPTION "String Utilities"
    CATEGORY "data"
    LICENSE "BSD"
    AUTHOR "Kon Lovett")

set(CHICKEN_OPTIONS -scrutinize -fixnum-arithmetic -inline -local
    -optimize-level 3 -debug-level 0 -no-procedure-checks)

chicken_add_module(unicode-utils unicode-utils.scm EXTENSION)
chicken_add_module(memoized-string memoized-string.scm EXTENSION
    OPTIONS -disable-interrupts
    DEPENDS unicode-utils)
chicken_add_module(to-hex to-hex.scm EXTENSION)
chicken_add_module(string-hexadecimal string-hexadecimal.scm EXTENSION
    DEPENDS to-hex)
chicken_add_module(string-utils string-utils.scm EXTENSION
    DEPENDS memoized-string unicode-utils string-hexadecimal)

chicken_install(EXTENSION string-utils MODULES)
