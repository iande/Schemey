(load "basics.scm")

(write "max of (34 53 12 39 31 83 16 3) is:")
(write (max 34 53 12 39 31 83 16 3))

(write "multiplying each of (34 53 12 39 31 83 16 3) by 2 is:")
(write (map (* 2) '(34 53 12 39 31 83 16 3)))

(write "divving each of (34 53 12 39 31 83 16 3) by 2 is:")
(write (map ((flip /) 2) '(34 53 12 39 31 83 16 3)))

(write "filtering out all elements of (34 53 12 39 31 83 16 3) less than 34 yields:")
(write (filter ((flip <) 34) '(34 53 12 39 31 83 16 3)))

(write "filtering out all elements of (34 53 12 39 31 83 16 3) greater than 34 yields:")
(write (filter ((flip >) 34) '(34 53 12 39 31 83 16 3)))


(write "partitioning elements less than 34 in (34 53 12 39 31 83 16 3) yields:")
(write (partition ((flip <) 34) '(34 53 12 39 31 83 16 3)))
