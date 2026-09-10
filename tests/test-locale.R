# Segmentation must not depend on the locale.
#
# In a non-UTF-8 locale (LC_ALL=C, the default on many servers) text arrives
# tagged "unknown" and R treats each byte as a character; the abbreviation
# regex in segment_text() then fails on any non-ASCII input. This was a real
# bug: the Japanese half of the paper's analyses would not run there.

# The failures below only appear when LC_CTYPE is not UTF-8, so force it here:
# run on the author's machine or an ordinary CI, this file would otherwise pass
# whether or not the fix is present.
.old_ctype <- Sys.getlocale("LC_CTYPE")
if (!identical(Sys.setlocale("LC_CTYPE", "C"), "C"))
  message("test-locale.R: could not switch LC_CTYPE to C; running as-is")
on.exit(Sys.setlocale("LC_CTYPE", .old_ctype), add = TRUE)

library(qualembed)

bytes <- function(v)
  unname(vapply(v, function(t) paste(as.integer(charToRaw(t)), collapse = "-"), ""))

x <- c(a = "昨日は雨。傘を忘れた。",
       b = "Dr. Smith arrived. It rained.")
want <- c("昨日は雨。", "傘を忘れた。",
          "Dr. Smith arrived.", "It rained.")

got <- segment_text(x, by = "sentence")$text
stopifnot(identical(bytes(got), bytes(want)))

# Word and character windows must also survive non-ASCII input.
stopifnot(nrow(segment_text(c(a = "one two three four five six"),
                            by = "words", size = 3)) == 2L)
stopifnot(nrow(segment_text(c(a = strrep("あ", 50)),
                            by = "chars", size = 20)) >= 2L)

cat("locale-independent segmentation: OK\n")

# Reading a file must not depend on the locale either. Excel writes "CSV UTF-8"
# with a byte-order mark, and a CP932 export read as UTF-8 has to stop rather
# than return three rows of raw bytes. Both used to differ between LC_ALL=C and
# a UTF-8 locale.

f <- tempfile(fileext = ".csv")
con <- file(f, "wb")
writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)),
           charToRaw('doc_id,segid,text\nd1,1,"昨日は雨だった。"\n')), con)
close(con)
b <- read_segments(f)
stopifnot(identical(b$doc_id[1], "d1"), b$n_char[1] == 8L)

p <- tempfile(fileext = ".csv")
con <- file(p, "wb")
writeBin(iconv("doc_id,segid,text\nd,1,昨日は雨だった。\n",
               "UTF-8", "CP932", toRaw = TRUE)[[1]], con)
close(con)
# It must stop with the explanation, not with whatever R says about multibyte
# strings: the raw message differs by locale and tells the reader nothing.
e <- try(read_segments(p), silent = TRUE)
stopifnot(inherits(e, "try-error"),
          grepl("could not be read as", as.character(e), fixed = TRUE))

# Naming the encoding must work, and must work the same in both locales.
q <- read_segments(p, encoding = "CP932")
stopifnot(identical(bytes(q$text[1]), bytes("昨日は雨だった。")))

# Windows writes CRLF, and a stray carriage return would ride along inside the
# last field of every row.
cr <- tempfile(fileext = ".csv")
con <- file(cr, "wb")
writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)),
           charToRaw("doc_id,segid,text\r\nd1,1,昨日は雨。\r\n")), con)
close(con)
crd <- read_segments(cr)
stopifnot(identical(bytes(crd$text[1]), bytes("昨日は雨。")), crd$n_char[1] == 5L)

# A newline inside a quoted field is one segment, not two.
nl <- tempfile(fileext = ".csv")
con <- file(nl, "wb")
writeBin(charToRaw('doc_id,segid,text\nd1,1,"一行目\n二行目"\nd1,2,"次"\n'), con)
close(con)
stopifnot(nrow(read_segments(nl)) == 2L)

# The plain-text and subtitle paths must handle the mark and the mismatch the
# same way the CSV path does; they used to go through readLines(encoding=),
# which only declares an encoding and neither strips the mark nor converts.

t1 <- tempfile(fileext = ".txt")
con <- file(t1, "wb")
writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw("昨日は雨。\n")), con)
close(con)
stopifnot(nchar(read_segments(t1)$text[1]) == 5L)

t2 <- tempfile(fileext = ".txt")
con <- file(t2, "wb")
writeBin(iconv("昨日は雨だった。\n", "UTF-8", "CP932", toRaw = TRUE)[[1]], con)
close(con)
e2 <- try(read_segments(t2), silent = TRUE)
stopifnot(inherits(e2, "try-error"),
          grepl("could not be read as", as.character(e2), fixed = TRUE))
stopifnot(identical(bytes(read_segments(t2, encoding = "CP932")$text[1]),
                    bytes("昨日は雨だった。")))

t3 <- tempfile(fileext = ".vtt")
con <- file(t3, "wb")
writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)),
           charToRaw("WEBVTT\n\n00:01.0 --> 00:03.0\n<v 田中>こんにちは\n")), con)
close(con)
v <- read_segments(t3, speaker = TRUE)
stopifnot(identical(bytes(v$speaker[1]), bytes("田中")),
          identical(bytes(v$text[1]), bytes("こんにちは")))

# A long Japanese speaker label is stripped by characters, not by bytes.
s <- read_segments(local({
  g <- tempfile(fileext = ".txt")
  writeLines("インタビュー参加者の田中太郎さん: 昨日は雨だった。", g, useBytes = TRUE)
  g
}), speaker = TRUE)
stopifnot(!is.null(s$speaker), !is.na(s$speaker[1]))

cat("locale-independent reading: OK\n")
