project(aoc_nim)

find_program(nimble
  NAMES nimble
  REQUIRED
)

add_custom_target(${PROJECT_NAME} ALL
  COMMAND ${nimble} install -y
  COMMAND ${nimble} lib
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
