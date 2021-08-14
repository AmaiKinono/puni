---
name: Bug report
about: Bug report about deletion commands
title: ''
labels: ''
assignees: ''

---

Please keep in mind that, due to the unique approach Puni takes, it can't
fix every use case in every language, or we'll end up with a lot of ad-hoc
tricks, which contradicts with the unified approach taken by Puni.

So, before you report a bug of the deletion commands, I'd like you to:

- Make sure you've read README thoroughly, and have at least a vague concept of
  how Puni works.

- Try `forward-sexp` and `backward-sexp` around the thing you want to delete,
  and get an idea of how they understand the syntactic structure around there.

  Here we are talking about the built-in `forward/backward-sexp` commands, not
  `puni-forward/backward-sexp`. When `puni-mode` is enabled, `C-M-f` and
  `C-M-b` are bind to the latter ones.

- Now, if you think there's any hope that it can be solved, post an issue with
  the investigations you've made.
