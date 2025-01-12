import Data.List (delete, nub, union)
import PA1Helper
import System.Environment (getArgs)

-- Main function: This is the entry point of the program. It retrieves command line arguments
-- to determine input and output file names. If arguments are not provided, it defaults to
-- "input.lambda" and "output.lambda". Then, it runs the program using `runProgram`, which processes
-- the input using the `reducer` function.
main :: IO ()
main = do
  arguments <- getArgs
  let inputFile = case arguments of
        inputArg : _ -> inputArg
        _ -> "input.lambda"
  let outputFile = case arguments of
        _ : outputArg : _ -> outputArg
        _ -> "output.lambda"
  runProgram inputFile outputFile reducer

-- Reducer function: This is the main processing function that reduces a lambda expression.
-- It first applies beta reduction (`betaReduce`) and then eta conversion (`etaConvert`) to the expression.
reducer :: Lexp -> Lexp
reducer = etaConvert . betaReduce

-- Eta conversion: Simplifies lambda expressions of the form (\x -> f x) to just `f` if `x`
-- does not appear in `f`. This function recursively processes the lambda expression, applying
-- the eta conversion rules wherever applicable.
etaConvert :: Lexp -> Lexp
etaConvert (Atom var) = Atom var
etaConvert (Apply expr1 expr2) = Apply (etaConvert expr1) (etaConvert expr2)
etaConvert (Lambda boundVar body@(Apply func arg))
  | boundVar `notElem` freeVars func || arg == Atom boundVar = etaConvert func
etaConvert (Lambda boundVar body) = Lambda boundVar (etaConvert body)

-- Beta reduction: Reduces lambda expressions by applying functions to their arguments.
-- It recursively reduces each part of the lambda expression and applies beta reduction when
-- encountering an application of a lambda function.
betaReduce :: Lexp -> Lexp
betaReduce (Atom var) = Atom var
betaReduce (Lambda boundVar body) = Lambda boundVar (betaReduce body)
betaReduce (Apply expr1 expr2) = betaReduceApply (betaReduce expr1) (betaReduce expr2)

-- Helper for beta reduction: Applies a lambda expression (`expr1`) to another expression (`expr2`),
-- handling the replacement of bound variables in the lambda body. Uses `alphaRename` to avoid variable
-- capture during substitution, ensuring unique variable names.
betaReduceApply :: Lexp -> Lexp -> Lexp
betaReduceApply (Lambda boundVar bodyExpr) expr2Reduced =
  betaReduce (replace newVar expr2Reduced renamedBodyExpr)
  where
    avoidVars = nub (allVars (Lambda boundVar bodyExpr) ++ freeVars expr2Reduced)
    Lambda newVar renamedBodyExpr = alphaRename (Lambda boundVar bodyExpr) avoidVars
betaReduceApply expr1Reduced expr2Reduced = Apply expr1Reduced expr2Reduced

-- Replace function: Substitutes occurrences of a target variable in an expression with another expression (`replacement`).
-- It carefully avoids replacing variables bound within a lambda expression to prevent variable capture.
replace :: String -> Lexp -> Lexp -> Lexp
replace targetVar replacement (Atom currentVar)
  | currentVar == targetVar = replacement
  | otherwise = Atom currentVar
replace targetVar replacement (Lambda boundVar body)
  | boundVar == targetVar = Lambda boundVar body -- Avoid replacing inside lambda where target is bound
  | otherwise = Lambda boundVar (replace targetVar replacement body)
replace targetVar replacement (Apply expr1 expr2) =
  Apply (replace targetVar replacement expr1) (replace targetVar replacement expr2)

-- Alpha renaming: Ensures that bound variables in lambda expressions have unique names, avoiding conflicts with free variables.
-- It generates a new variable name not present in `avoidVars` and replaces the old bound variable in the expression.
alphaRename :: Lexp -> [String] -> Lexp
alphaRename (Atom var) _ = Atom var
alphaRename (Apply expr1 expr2) avoidVars =
  Apply (alphaRename expr1 avoidVars) (alphaRename expr2 avoidVars)
alphaRename (Lambda boundVar body) avoidVars
  | boundVar == newBoundVar = Lambda newBoundVar (alphaRename body updatedAvoidVars)
  | otherwise = Lambda newBoundVar (alphaRename updatedBody updatedAvoidVars)
  where
    newBoundVar = findName boundVar avoidVars
    updatedAvoidVars = newBoundVar : avoidVars
    updatedBody = replace boundVar (Atom newBoundVar) body

-- Finds a new variable name that is not in the list of variables to avoid (`avoidVars`).
-- Appends "1" to the variable name if needed to generate a unique name.
findName :: String -> [String] -> String
findName varName avoidVars
  | varName `notElem` avoidVars = varName
  | otherwise = findName (varName ++ "1") avoidVars

-- Computes the list of free variables in a lambda expression. A variable is considered free if it is not bound by a lambda.
freeVars :: Lexp -> [String]
freeVars (Atom var) = [var]
freeVars (Lambda boundVar body) = delete boundVar (freeVars body) -- Remove the bound variable from the free variables of the body
freeVars (Apply expr1 expr2) = freeVars expr1 `union` freeVars expr2

-- Computes all variables (both bound and free) present in a lambda expression.
allVars :: Lexp -> [String]
allVars (Atom var) = [var]
allVars (Lambda boundVar body) = boundVar : allVars body
allVars (Apply expr1 expr2) = allVars expr1 ++ allVars expr2
