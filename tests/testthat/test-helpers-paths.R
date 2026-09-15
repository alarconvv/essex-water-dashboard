# tests/testthat/test-helpers-paths.R

test_that("resolve_config_path defaults to config.yml when env var is unset", {
  withr::local_envvar(ESSEXWATER_CONFIG_PATH = NA)
  expect_equal(resolve_config_path(), "config.yml")
})

test_that("resolve_config_path treats an empty env var as unset", {
  withr::local_envvar(ESSEXWATER_CONFIG_PATH = "")
  expect_equal(resolve_config_path(), "config.yml")
})

test_that("resolve_config_path honors ESSEXWATER_CONFIG_PATH", {
  withr::local_envvar(ESSEXWATER_CONFIG_PATH = "/tmp/test-config.yml")
  expect_equal(resolve_config_path(), "/tmp/test-config.yml")
})

test_that("resolve_config_path accepts a custom default", {
  withr::local_envvar(ESSEXWATER_CONFIG_PATH = NA)
  expect_equal(resolve_config_path(default = "other.yml"), "other.yml")
})

test_that("resolve_db_path defaults to data/essexwater.sqlite", {
  withr::local_envvar(ESSEXWATER_DB_PATH = NA)
  expect_equal(resolve_db_path(), "data/essexwater.sqlite")
  withr::local_envvar(ESSEXWATER_DB_PATH = "")
  expect_equal(resolve_db_path(), "data/essexwater.sqlite")
})

test_that("resolve_db_path honors ESSEXWATER_DB_PATH", {
  withr::local_envvar(ESSEXWATER_DB_PATH = "/tmp/fixture.sqlite")
  expect_equal(resolve_db_path(), "/tmp/fixture.sqlite")
})

test_that("the two env vars are independent", {
  withr::local_envvar(
    ESSEXWATER_DB_PATH = "/tmp/fixture.sqlite",
    ESSEXWATER_CONFIG_PATH = NA
  )
  expect_equal(resolve_config_path(), "config.yml")
  expect_equal(resolve_db_path(), "/tmp/fixture.sqlite")
})
