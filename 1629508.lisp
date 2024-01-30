#|
Name: Qi Zhou
Student ID: 1629508
COMPUT 325 LEC B1 Winter 2024
|#
#|QUESTION 1.

This function takes in a list and counts the total number of atoms in the 
possibly nested list.

How this function works:
    It employs two base cases: if the input list is empty, 
    it returns 0, and if the first element of the list is an atom, 
    it returns 1. In the recursive case, when the first element is a sublist,
    the function sums the counts of atoms in the first element (car) 
    and the rest of the list (cdr).

test cases:
    (xcount '(() (1 2 (1 2)) 4)) => 5
    (xcount ()) => 0
    (xcount '(NIL a)) => 1
    (xcount '(a)) => 1
|#

(defun xcount (L)
    (if (null L)
        0
        (if (atom L)
            1
            (+ (xcount (car L))  (xcount (cdr L)))
        )
    )
)

#|QUESTION 2.

The function remove-duplicate takes a list 'x' containing atoms and removes any repeated occurrences,
preserving the original order of elements in the resulting list.

How remove-duplicate works:
    A supporting function: is-member is used to check if an element is already in the list.

    remove-duplicate checks if the first element of the list is a member of the rest of the list.
    If it is, it calls remove-duplicate on the rest of the list. If it is not,
    then it construct the list including the first element.

test cases:
    (remove-duplicate '(a b c)) => (a b c)
    (remove-duplicate '(a b b c)) => (a b c)
    (remove-duplicate '()) => NIL
    (remove-duplicate '(a)) => (a)
|#

(defun is-member(x L)
    (if (null L)
        nil
        (if (equal x (car L))
            t
            (is-member x (cdr L))
        )
    )
)

(defun remove-duplicate(x)
    (if (null x)
        x
        (if (is-member (car x) (cdr x))
            (remove-duplicate (cdr x))
            (cons (car x) (remove-duplicate (cdr x)))
        )
    )
)

#|QUESTION 3 PART A.
This fuction takes two lists:L1 and L2 and alternatively selects 
elements from L2 and L1 to form a new list.

How this function works:
    the base cases for this functionas are when either of the lists is empty.
    the recursive case is when both lists are non-empty, it will constrcut the list using the first
    element of each list, and then call mix on the rest of the lists.

test cases:
    (mix '(a b c) '(d e f)) => (d a e b f c)
    (mix '((a) (b c)) '(d e f g h))  => (d (a) e (b c) f g h)

|#

(defun mix (L1 L2) 
    (if (null L2)
        L1
        (if (null L1)
            L2
            (cons (car L2) (cons (car L1) (mix (cdr L1) (cdr L2))))
        )
    )
)
#|QUESTION 3 PART B.

This fuction takes a list L and returns a list of two sublists
the first one of which is the list of elements in L at even positions 
and the second is the list of elements in L at odd positions. 
If L is empty, then a list of two empty lists is returned. 

How this function works:
    There are two helper functions:
        1. get_even: takes in a list and returns a list of elements at even positions.
        2. get_odd: takes in a list and returns a list of elements at odd positions.
    The split function calls these two functions and returns a list of the two lists.
    

test cases:
    (split '(1 2 3 4 5 6)) => ((1 3 5) (2 4 6))
    (split '((a) (b c) (d e f) g h))  => (((b c) g) ((a) (d e f) h))
|#

(defun get_even (L)
    (if (or (null L) (null (car (cdr L))))
        nil
        (cons (car (cdr L)) (get_even (cdr (cdr L))))
    )
)

(defun get_odd(L)
    (if (or (null L) (null (car L)))
        nil
        (cons (car L) (get_odd (cdr (cdr L))))
    )
)

(defun split (L)
    (list (get_even L) (get_odd L))
)


#|QUESTION 4.

The function subsets takes in a list L and a non-negative number S, and
returns a list of all subsets of L of size S.

How the function works:
there are two help functions:
    1. size: returns the count of elements in a list.
    2. build-set: takes in a list which is the accumulator of what has been build for the sublist, a list of remaining elements from the orginal list, and a number n which indicates 
        how many more elements we need to add to the list we are building. It returns a list of all subsets of size n that can be built from the remaining list.
        the base case it when n is 0, in which case it means we are done building the list, and it returns the list of the sublists we have built.
        the recurvise case always consider two cases:
            1. Include the first element of the remaining list
            2. Do not include the first element of the remaining list
        This function is mainly based on Pascal’s Identity, i.e. ncr = n-1cr + n-1cr-1

The subsets function checks if S is 0 or greater than the size of L, and returns nil if either of these is true.
Otherwise, it calls build-set to get all the subsets when the first element of L is included, and then recursievely calls subsets to get all subsets
for the case when the first element of L is not included. Then it combines the two lists and returns the result.

test cases:
    (subsets '(a b c) 2) => ((a b) (a c) (b c))
    (subsets '(a b c) 3) => ((a b c))
    (subsets '(a b c) 4) => NIL
    (subsets '(a b c d e f g) 3) => ((a b c) (a b d) (a b e) (a b f) (a b g) (a c d) (a c e) (a c f) (a c g) 
                                    (a d e) (a d f) (a d g) (a e f) (a e g) (a f g) (b c d) (b c e) (b c f) 
                                    (b c g) (b d e) (b d f) (b d g) (b e f) (b e g) (b f g) (c d e) (c d f)
                                    (c d g) (c e f) (c e g) (c f g) (d e f) (d e g) (d f g) (e f g))
|#


(defun size(L)
    (if (null L)
        0
        (+ 1 (size (cdr L)))
    )
)

(defun build-set(building remaining n)
    
    (if (equal n 0)
        (list building )
        (if (=(size remaining) n)
            (build-set (append building (list (car remaining))) (cdr remaining) (- n 1)) 
            (append 
                (build-set (append building (list (car remaining))) (cdr remaining) (- n 1)) 
                (build-set building (cdr remaining) n)
            )
        )
    )
)

(defun subsets(L S)
    (if (or (equal S 0) (> S (size L)))
        nil
        (append
                (build-set (list (car L)) (cdr L) (- S 1))
                (subsets (cdr L) S)
            )
        
    )
)


#|QUESTION 5.
This function takes a list, denoted as L, along with two expressions, E1 and E2. 
It serves to replace all occurrences of E1 within the list L with the expression E2.

How this function works:
    If the first element of the list equal to E1, then it replaces it with E2, and recursively calls this function on
    the rest of the list L.
    If it does not match with E1, then it checks if the first element is a sublist, and if it is, it calls this function on the sublist.
    Finally, if it does not match with E1 and is not a sublist, then it just returns the first element of the list and calls this function on the rest of the list.

test cases:
    (substitute-exp 'a 'b '(a b c)) => (b b c)
    (substitute-exp 'a 'b '(a (a b) (a b) (b))) => (b (b b) (b b) (b))
    (substitute-exp '(a b) 'b '(a (a 2) b (1 2 a) (a b) (a (a b)))) => (a (a 2) b (1 2 a) b (a b))

|#


(defun substitute-exp(E1 E2 L)
    (
        cond
        ((null L) nil)
        ((equal(car L) E1) (cons E2 (substitute-exp E1 E2 (cdr L))))
        ((not (atom (car L))) (cons (substitute-exp E1 E2 (car L)) (substitute-exp E1 E2 (cdr L))))
        (t (cons (car L) (substitute-exp E1 E2 (cdr L))))
    )
)

#|QUESTION 6.

This function takes a non-nested list L and returns the number of distinct elements in L.

How this function works:
    It uses a helper function is-member which was defined in question 2 to check if an element is already in the list.
    If the first element of the list is in the rest of the list then we can simply drop it and recursively call this 
    function on the rest of the list.

test cases:
    (my-count '(a b c c c c c)) => 3
    (my-count '()) => 0
    (my-count '(a a b c)) => 3
|#

(defun my-count (L)
    (if (null L)
        0
        (if (is-member(car L) (cdr L))
            (my-count (cdr L))
            (+ 1 (my-count (cdr L)))
        )
    )
)
#|QUESTION 7.
reached:
This functions takes in two input x and L.
L includes a list of pairs of the form (a b), which represents that b is reachable from a.
The function returns a list of all elements that are reachable from x.

How this function works:
    This function uses a helper function get-reached which takes in 4 inputs:
        1. x: the current element we are trying to find the reachable elements for
        2. origional-x: the element we started with. this stays the same..
        3. travelling-list: the list of pairs we need to look at
        4. full-list: the list of pairs we have. this stays the same.
    helper function get-reached works as follows:
    lets denoted the first element of the first pair in list as A and the second element as B.
    If x equals to A, that means B is reachable to x, so then the function calls itself to get the list of 
    elements that are reachable from B, with the travelling-list being the full-list.
    and then calls itself to keep getting the list of elements that are reachable from the x,
    with the travelling-list being the rest of the list, since we already looked at the first pair for x.
    and appends the results together with B.
    If A is not equal to x, or x equals to the original starting x,
    then it calls itself to keep getting the list of elements that are reachable from the x,
    
    It uses another helper function remove-duplicate which was defined in question 2 to remove
    any repeated pairs before calling get-reached to pass in the full-list and the travelling-list.
    
    function 'reached' calls remove-duplicate to get the new list without any repeated pairs, then calls get-reached
    to get the list of elements that are reachable from x.
test cases:
    (reached 1 '((7 8) (1 2) (3 6) (2 3) (1 7))) => (2 3 6 7 8)
    (reached 'google '( (google shopify) (google shopify) (google aircanada) (amazon aircanada))) => (SHOPIFY AIRCANADA)
    (reached 'a '((a a) (a b))) => (b)
|#

(defun get-reached(x origional-x travelling-list full-list)
(let ((A (car (car travelling-list))) (B (car (cdr (car travelling-list))) ))
    (cond 
        ((null travelling-list)  nil)
        ((and (equal x A) (not (equal B origional-x)))
            (append (append (list B) (get-reached B origional-x full-list full-list)) 
            (get-reached x origional-x (cdr travelling-list) full-list))
        )
        (t (get-reached x origional-x (cdr travelling-list) full-list))
        )
    
))

(defun reached(x L)
    (let ((new-list (remove-duplicate L)))
        (get-reached x x new-list new-list)
    )
    
)


#|QUESTION 7.
rank:
This function takes in two inputs: S and L.
S is a list of atoms,
L includes a list of pairs of the form (a b), which represents that b is reachable from a.
The function returns a sorted list of S based on how many times they are reached.

How this function works:
This function uses some helper functions:
    1. get-ranks: takes in a web and a list of pairs, and returns the number of times the web is reached.
    2. get-rank-pairs: takes in a list of webs and a list of pairs, and returns a list of pairs of the form (web rank).
    This function calls get-ranks to get the ranks for each web.
    3. my-sort: takes in a list of pairs of the form (web rank) and sorts them based on the rank.
    4. greater-than: takes in two pairs of the form (web rank) and returns true if the rank of the first pair is greater than the rank of the second pair.
    This is the comparator function used by my-sort.
    5. get-webs: takes in a list of pairs of the form (web rank) and returns a list of webs. 
    6. remove-duplicate: takes in a list and removes any repeated occurrences. This was defined in question 2.
With the help of these functions, rank calls get-rank-pairs with the result from remove-duplicate on L to get a list of pairs of the form (web rank),
then calls my-sort to sort the list based on the rank, and finally calls get-webs to get a list of webs.
    
test cases:
(rank '(google shopify aircanada amazon delta) '((google shopify) (google aircanada) (amazon aircanada) (aircanada delta) (google google))) => (AIRCANADA SHOPIFY DELTA GOOGLE AMAZON)
|#



(defun get-ranks(web L) 
    (cond
        ((null L) 0)
        ((and (equal web (car (cdr (car L)))) (not (equal web (car (car L)))) )
            (+ 1 (get-ranks web (cdr L)))
        )
        (t (get-ranks web (cdr L)))
    )

)

(defun get-rank-pairs(S L)
    (if (null S)
        nil
        (cons 
            (cons (car S) (get-ranks (car S) L))
            (get-rank-pairs (cdr S) L)
        )
    )
)

(defun my-sort (L)
    (sort L 'greater-than)
)

(defun greater-than (L1 L2)
            (> (cdr L1) (cdr L2))
)

(defun get-webs (L)
    (if (not (null L))
        (cons (car (car L)) (get-webs (cdr L)))
    )
) 

(defun rank(S L)
    (get-webs(my-sort (get-rank-pairs S (remove-duplicate L))))
)


