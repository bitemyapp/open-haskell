aparser_file=$1

cat > ./SExpr.hs <<EOF

{-# OPTIONS_GHC -Wall #-}
EOF

cat $aparser_file >> ./SExpr.hs

runhaskell HW11Test.hs
