emails = c("bob@test.com",
           "bob",
           "bob @ test.com",
           "bob@testcom"
           )

ans = sapply(emails, function(x) try(validate_email(x)))

stopifnot(!is(ans[[1]], "try-error"))
stopifnot(all(sapply(ans[2:4], is, "try-error")))

assertError(validate_email(emails))
