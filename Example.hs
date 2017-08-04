import ENFA

-- A ENFA accepting all the binary strings that end in 01
enfa :: ENFA Int Char
enfa = trans (0, Any, 0) . trans (0, Symbol '0', 1) .
       trans (1, Symbol '1', 2). accept 2 $ initENFA 0
