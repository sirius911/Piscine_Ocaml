#! /bin/bash

echo -n "Effacement de tous les .cmx .cmi et .o et a.out..."
find ./ -type f \( -name "*.cmx" -o -name "*.cmi" -o -name "*.o" -o -name "a.out" \) -exec rm {} \;
echo " OK"