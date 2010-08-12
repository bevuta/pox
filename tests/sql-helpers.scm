(use test)
(load-relative "../sql-helpers")
(import sql-helpers)

(test-group "alist->sql-update/insert"
  (test '("INSERT INTO \"widgets\" (\"foo\", \"baz\") VALUES ($1, $2)" "bar" "qux")
    (alist->sql-update/insert "widgets" '((foo . "bar") (baz . "qux"))))
  (test '("UPDATE \"widgets\" SET \"foo\" = $1, \"baz\" = $2 WHERE \"id\" = $3" "bar" "qux" 10) 
    (alist->sql-update/insert "widgets" '((foo . "bar") (baz . "qux") (id . 10))))
  (test '("UPDATE \"widgets\" SET \"foo\" = $2, \"baz\" = $3 WHERE \"zing\" = $4 AND (\"rev\" = $1)" 2 "bar" "qux" 10) 
    (alist->sql-update/insert "widgets" '((foo . "bar") (baz . "qux") (zing . 10))
			      pkey: 'zing conditions: '("\"rev\" = $1" 2))))

(test-group "alist->sql-insert"
  (test '("INSERT INTO \"foo\" (\"foo\", \"baz\") VALUES ($1, $2)" "bar" 2) (alist->sql-insert "foo" '((foo . "bar") (baz . 2)))))

(test-group "alist->sql-update"
  (test '("UPDATE \"fasel\" SET \"foo\" = $2, \"bar\" = $3 WHERE \"lol\" = $4 AND (\"whoops\" = $1)" "heh" "zing" "bang" 42) 
    (alist->sql-update "fasel" '((lol . 42) (foo . "zing") (bar . "bang")) pkey: 'lol conditions: '("\"whoops\" = $1" "heh"))))