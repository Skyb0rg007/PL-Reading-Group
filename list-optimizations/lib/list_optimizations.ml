
module type ListLike = List_intf.S

module Adt = Adt
module Scott : ListLike = Scott
module Church : ListLike = Church
module Hughes : ListLike = Hughes
module Rwr : ListLike = Rwr
module Iter : ListLike = Iter
module Lazy : ListLike = Lazy
