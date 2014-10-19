cmake_minimum_required(VERSION 2.8)

project(fmt C)

find_package(Chicken REQUIRED)
find_chicken_extension(srfi-13 REQUIRED)
find_chicken_extension(srfi-69 REQUIRED)
find_chicken_extension(utf8 REQUIRED)

define_chicken_extension(${PROJECT_NAME}
    VERSION 1.4.1
    DESCRIPTION "Combinator Formatting"
    CATEGORY "io"
    LICENSE "BSD"
    AUTHOR "Alex Shinn")

set(CHICKEN_OPTIONS -optimize-level 2 -debug-level 0)

add_chicken_module(fmt fmt-chicken.scm EXTENSION)

add_chicken_module(fmt-c fmt-c-chicken.scm EXTENSION
    DEPENDS fmt-c.scm fmt)

add_chicken_module(fmt-js fmt-js-chicken.scm EXTENSION
    DEPENDS fmt-js.scm fmt fmt-c)

add_chicken_module(fmt-color fmt-color-chicken.scm EXTENSION
    DEPENDS fmt-color.scm fmt)

add_chicken_module(fmt-unicode fmt-unicode-chicken.scm EXTENSION
    DEPENDS fmt)

# TODO: add install documentation

install_chicken_modules(${PROJECT_NAME})
