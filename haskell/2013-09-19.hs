module HS20130919 where

suffixes lst = suf lst []  -- define a helper function to add an accumulator
                where
                    suf [] res = res
                    suf (x:xs) res = suf xs ((x:xs) : res)

prefixes lst = pre lst []
                where
                    pre [] res = res
                    pre (x:xs) [] = pre xs [[x]]
                    pre (x:xs) res = pre xs $ ((head res) ++ [x]) : res

infixes lst = foldl (++) [] $
                map suffixes (prefixes lst)
