context("demo")
roc <- demo_roclet()


test_that("description and no details", {
  out <- roc_proc_text(roc, "
    #' @demo aaa bbb ccc
    NULL")

  expect_equivalent(get_tag(out, "demo"), "aaa bbb ccc")
  expect_equivalent(get_tag(out, "details"), "")
})


test_that("description and multiline details", {
  out <- roc_proc_text(roc, "
    #' @demo aaa bbb ccc
    #' @details ddd eee
    #'   fff ggg hhh
    NULL")

  expect_equivalent(get_tag(out, "demo"), "aaa bbb ccc")
  expect_equivalent(get_tag(out, "details"), "ddd eee fff ggg hhh")
})


test_that("missing description", {
  expect_warning(roc_proc_text(roc, "NULL"))
})




