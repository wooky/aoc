find_program(zig
  NAMES zig
  REQUIRED
)

file(GLOB_RECURSE ZIG_SOURCES *.zig)
set(ZIG_LIB "${CMAKE_BINARY_DIR}/libaoc_zig.so")

add_custom_command(
  OUTPUT ${ZIG_LIB}
  COMMAND ${zig} build
  DEPENDS ${ZIG_SOURCES}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
add_custom_target(aoc_zig ALL
  DEPENDS ${ZIG_LIB}
)
