find_program(gprbuild
  NAMES gprbuild
  REQUIRED
)

file(GLOB_RECURSE ADA_SOURCES *.adb *.ads *.gpr)
set(ADA_LIB "${CMAKE_BINARY_DIR}/libaoc_ada.so")

add_custom_command(
  OUTPUT ${ADA_LIB}
  COMMAND ${gprbuild}
  DEPENDS ${ADA_SOURCES}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
add_custom_target(aoc_ada ALL
  DEPENDS ${ADA_LIB}
)
