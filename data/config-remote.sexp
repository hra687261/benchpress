
(task
  (name z3-all-remote)
  (synopsis "run z3 on smtlib")
  (action
    (run_provers
      (dirs ($BENCHPRESS_REMOTE_DIR))
      (provers (z3))
      (timeout 10)
      (remote
        (host $BENCHPRESS_REMOTE_HOST)
        (username $BENCHPRESS_REMOTE_USERNAME)
      )
    )
  )
)
