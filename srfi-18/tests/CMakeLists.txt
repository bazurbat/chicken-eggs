project(srfi-18-tests C)

add_chicken_test(simple-thread simple-thread-test.scm)
add_chicken_test(mutex mutex-test.scm)

add_chicken_interpreted_test(simple-thread SCRIPT simple-thread-test.scm)
add_chicken_interpreted_test(mutex SCRIPT mutex-test.scm)

add_dependencies(tests simple-thread mutex)
