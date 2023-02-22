
module type S = List_intf.S

module Adt : S = Adt
module Scott : S = Scott
module Church : S = Church
module Hughes : S = Hughes
module Rwr : S = Rwr
