project(aoc_zig)

find_program(zig
  NAMES zig
  REQUIRED
)

add_custom_target(${PROJECT_NAME} ALL
  COMMAND ${zig} build
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
