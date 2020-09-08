project(libmsis)
add_library(msis SHARED)
target_sources(msis PRIVATE nrlmsise00.f)
