module Optimizations where

import Types
import ConstantsManagement hiding(eval,addConst)-- constants optimizations
import CodeManagement      -- code optimizations

optimize p = Program $ optimizeMainProc (mainProc p)

optimizeMainProc (Proc id params decs stmts ln) =
     optimizeCode (Proc id params (optimizeConstants decs) stmts ln)