package edu.colorado.csci3155.project1

object StackMachineCompiler {



    /* Function compileToStackMachineCode
        Given expression e as input, return a corresponding list of stack machine instructions.
        The type of stack machine instructions are in the file StackMachineEmulator.scala in this same directory
        The type of Expr is in the file Expr.scala in this directory.
     */
    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction] = {
        //TODO: Your code here
        e match {
    		case Const(f) => List(PushI(f))
            
            case Ident(s) => List(StoreI(s))
            
    		case Plus(e1, e2) => {
    			val lst1 = compileToStackMachineCode(e1)
    			val lst2 = compileToStackMachineCode(e2)
    			lst1 ++ lst2 ++ List(AddI)
    		}
            
    		case Minus(e1, e2) => {
    			val lst1 = compileToStackMachineCode(e1)
    			val lst2 = compileToStackMachineCode(e2)
    			lst1 ++ lst2 ++ List(SubI)
    		}
            
    		case Mult(e1, e2) => {
    			val lst1 = compileToStackMachineCode(e1)
    			val lst2 = compileToStackMachineCode(e2)
    			lst1 ++ lst2 ++ List(MultI)
    		}
    		
            case Div(e1, e2) => {
    			val lst1 = compileToStackMachineCode(e1)
    			val lst2 = compileToStackMachineCode(e2)
    			lst1 ++ lst2 ++ List(DivI)
    		}
    		
            case Log(e1) => {
    			val lst1 = compileToStackMachineCode(e1)
    			lst1 ++ List(LogI)
    		}
            
            case Exp(e1) => {
    			val lst1 = compileToStackMachineCode(e1)
    			lst1 ++ List(ExpI)
    		}
    		
            case Sine(e1) => {
    			val lst1 = compileToStackMachineCode(e1)
    			lst1 ++ List(SinI)
    		}
    		
            case Cosine(e1) => {
    			val lst1 = compileToStackMachineCode(e1)
    			lst1 ++ List(CosI)
            }
            
            case Let(identifier, e1, e2) =>  {
                val lst1 = compileToStackMachineCode(e1) 
                val lst2 = compileToStackMachineCode(e2)
                lst1 ++ List(LoadI(identifier)) ++ lst2
    		}
    	}
    }
}