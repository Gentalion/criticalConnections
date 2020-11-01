(defun test (func testSamples)
    (testWithCounter func 1 testSamples)
)

(defun testWithCounter (func counter testSamples)
    (cond
        ((null testSamples) 
            NIL)
        (T 
            (princ "Test ")
            (princ counter)
            (terpri)
            (princ "Input: ")
            (princ (car testSamples))
            (terpri)
            (princ "Output: ")
            (princ (funcall func (car testSamples)))
            (terpri)
            (terpri)
            (testWithCounter func (+ counter 1) (cdr testSamples)))
    )
)

(defun testHardcoded (testSamples)
    (testHardcodedWithCounter 1 testSamples)
)

(defun testHardcodedWithCounter (counter testSamples)
    (cond
        ((null testSamples) 
            NIL)
        (T 
            (let 
                (
                    (ioc (parseInputInOutConnections (car testSamples)))
                ) 
                ;(findAllPaths (car ioc) (cadr ioc) (caddr ioc))
                (princ "Test ")
                (princ counter)
                (terpri)
                (princ "Input: ")
                (princ (car testSamples))
                (terpri)
                (princ "All Paths: ")
                (format t "~{~a~^, ~}" (findAllPaths (car ioc) (cadr ioc) (caddr ioc)))
                (terpri)
                (princ "Critical connections: ")
                (let
                    (
                        (criticalConnections (findCriticalConnections (car ioc) (cadr ioc) (caddr ioc)))
                    )
                    (cond
                        ((null criticalConnections)
                            (princ "No"))
                        (T
                            (princ "Yes - ")
                            (format t "~{~a~^, ~}" criticalConnections))
                    )
                )
                (terpri)
                (terpri)
                (testHardcodedWithCounter (+ counter 1) (cdr testSamples))))
    )
)

(defun parseInputInOutConnections (input)
    (cond
        ((null input)
            (cons NIL (cons NIL (cons NIL NIL))))
        (T
            (let
                (
                    (parseInputCdr (parseInputInOutConnections (cdr input)))
                )
                (cond
                    ((or (eq (caar input) 'IN) (eq (cadar input) 'IN))
                        (cons (cons (other 'IN (car input)) (car parseInputCdr)) (cons (cadr parseInputCdr) (cons (caddr parseInputCdr) NIL))))
                    ((or (eq (caar input) 'OUT) (eq (cadar input) 'OUT))
                        (cons (car parseInputCdr) (cons (cons (other 'OUT (car input)) (cadr parseInputCdr)) (cons (caddr parseInputCdr) NIL))))
                    (T
                        (cons (car parseInputCdr) (cons (cadr parseInputCdr) (cons (cons (car input) (caddr parseInputCdr)) NIL))))
                )
            ))
    )
)

(defun findCriticalConnections (in out connections)
    (findCriticalConnectionsWorker
        (findAllPathsWithoutSugar in out connections)
        connections
        NIL)
)

(defun findCriticalConnectionsWorker (paths connections acc)
    (cond
        ((null connections)
            acc)
        ((reduce
            #'simpleAnd 
            (mapcar 
                (lambda (x) (isUsingConnection (car connections) x))
                paths)
            )
            
            (findCriticalConnectionsWorker paths (cdr connections) (cons (car connections) acc)))
        (T
            (findCriticalConnectionsWorker paths (cdr connections) acc))
    )
)

(defun simpleAnd (x y) (and x y))

(defun isUsingConnection (connection path)
    (cond
        ((null path)
            NIL)
        ((null (cdr path))
            NIL)
        ((or 
            (and 
                (eq (car connection) (car path)) 
                (eq (cadr connection) (cadr path))
            )
            (and
                (eq (cadr connection) (car path))
                (eq (car connection) (cadr path))
            ))
            T)
        (T
            (isUsingConnection connection (cdr path)))
    )
)

(defun findAllPaths (in out connections)
    (mapcar
        (lambda (x) (cons 'IN (reverse (cons 'OUT x))))
        (findAllPathsIterate out connections (mapcar #'(lambda (x) (cons x NIL)) in)))
)

(defun findAllPathsWithoutSugar (in out connections)
    (findAllPathsIterate out connections (mapcar #'(lambda (x) (cons x NIL)) in))
)

(defun findAllPathsIterate (goals connections pathsPrevIter)
    ;(princ pathsPrevIter)
    ;(terpri)
    (let
        (
            (pathsCurIter (findAllPathsIter goals connections pathsPrevIter))
        )
        ;(print pathsCurIter)
        ;(print (equal pathsPrevIter pathsCurIter))
        (cond
            ((equal pathsPrevIter pathsCurIter)
                pathsCurIter)
            (T
                (findAllPathsIterate goals connections pathsCurIter))
        )
    )
)

(defun findAllPathsIter (goals connections pathsPrevIter)
    (extendAllPaths goals connections pathsPrevIter NIL)
)

(defun extendAllPaths (goals connections pathsToExtend acc)
    (cond
        ((null pathsToExtend)
            (reverse acc))
        ((member (caar pathsToExtend) goals)
            (extendAllPaths goals connections (cdr pathsToExtend) (cons (car pathsToExtend) acc)))
        (T
            (extendAllPaths goals connections (cdr pathsToExtend) (append (extendPath (car pathsToExtend) connections NIL) acc)))
    )
)

(defun extendPath (pathToExtend connections acc)
    (cond
        ((null connections)
            acc)
        ((and (member (car pathToExtend) (car connections)) (not (member (other (car pathToExtend) (car connections)) pathToExtend)))
            (extendPath pathToExtend (cdr connections) (cons (cons (other (car pathToExtend) (car connections)) pathToExtend) acc)))
        (T
            (extendPath pathToExtend (cdr connections) acc))
    )
)

(defun other (one pair)
    (cond
        ((eq (car pair) one) 
            (cadr pair))
        (T
            (car pair))
    )
)

;(test #'(lambda (x) (let ((ioc (parseInputInOutConnections x))) (findAllPaths (car ioc) (cadr ioc) (caddr ioc)))) 
(testHardcoded
    '(
        () 
        ((IN 1) (OUT 1)) ((IN 1) (1 2) (OUT 2)) 
        ((IN 1) (1 2) (2 3) (OUT 3)) 
        ((IN 1) (1 2) (2 3) (3 4) (OUT 4))
        ((IN 3) (1 2) (2 4) (1 IN) (1 3) (4 5) (5 OUT))
        ((IN 1) (OUT 4) (1 2) (2 4) (1 3) (3 4))
    ))