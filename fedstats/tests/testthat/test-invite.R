# Invite crypto: signed invites, tamper/forgery detection, expiry, and the
# registration-callback signature. These protect the onboarding trust chain.

test_that("a freshly created invite verifies and round-trips its payload", {
  kp  <- fed_keypair()
  sid <- fed_sid(); tok <- fed_token()
  inv <- fed_invite_create("SweSpine", "coord.ts.net:8731", sid, tok, kp$private,
                           name = "Denmark", ttl_days = 7)
  p <- fed_invite_parse(inv)
  expect_true(p$ok)
  expect_false(p$expired)
  expect_equal(p$payload$study, "SweSpine")
  expect_equal(p$payload$sid,   sid)
  expect_equal(p$payload$tok,   tok)
  expect_equal(p$payload$name,  "Denmark")
  expect_equal(p$pk, kp$public)
})

test_that("a tampered invite fails signature verification", {
  kp  <- fed_keypair()
  inv <- fed_invite_create("S", "c:8731", fed_sid(), fed_token(), kp$private)
  # flip a character in the middle of the envelope
  mid <- nchar(inv) %/% 2L
  ch  <- substr(inv, mid, mid)
  swap <- if (ch == "A") "B" else "A"
  tampered <- paste0(substr(inv, 1, mid - 1), swap, substr(inv, mid + 1, nchar(inv)))
  p <- fed_invite_parse(tampered)
  expect_false(p$ok)
})

test_that("wrong prefix is rejected", {
  expect_false(fed_invite_parse("not-an-invite")$ok)
  expect_false(fed_invite_parse("FEDSTAT2.@@@notbase64@@@")$ok)
})

test_that("an expired invite is flagged expired", {
  kp  <- fed_keypair()
  now <- 1000000L
  inv <- fed_invite_create("S", "c:8731", fed_sid(), fed_token(), kp$private,
                           ttl_days = 1, now = now)
  p <- fed_invite_parse(inv, now = now + 2L * 86400L)
  expect_false(p$ok)
  expect_true(p$expired)
})

test_that("sign/verify detached signatures", {
  kp  <- fed_keypair()
  msg <- "register:s_123"
  sig <- fed_sign(msg, kp$private)
  expect_true(fed_verify(msg, sig, kp$public))
  expect_false(fed_verify("tampered", sig, kp$public))
  other <- fed_keypair()
  expect_false(fed_verify(msg, sig, other$public))  # wrong key
})

test_that("registration message is stable and verifiable", {
  kp  <- fed_keypair()
  m1  <- fed_register_message("s_1", "http://100.1.1.1:8000", kp$public, 1700000000L)
  m2  <- fed_register_message("s_1", "http://100.1.1.1:8000", kp$public, 1700000000L)
  expect_identical(m1, m2)                       # deterministic bytes
  sig <- fed_sign(m1, kp$private)
  expect_true(fed_verify(m1, sig, kp$public))
})

test_that("fingerprint is stable and public-key derivation is consistent", {
  kp <- fed_keypair()
  expect_identical(fed_fingerprint(kp$public), fed_fingerprint(kp$public))
  expect_identical(fed_public_key(kp$private), kp$public)
})
