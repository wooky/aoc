find_program(nimble
  NAMES nimble
  REQUIRED
)

file(GLOB_RECURSE NIM_SOURCES *.nim *.nimble)
set(NIM_LIB "${CMAKE_BINARY_DIR}/libaoc_nim.so")

add_custom_command(
  OUTPUT ${NIM_LIB}
  COMMAND ${nimble} install -y
  COMMAND ${nimble} lib
  DEPENDS ${NIM_SOURCES}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
add_custom_target(aoc_nim ALL
  DEPENDS ${NIM_LIB}
)
