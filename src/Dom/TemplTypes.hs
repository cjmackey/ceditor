module Dom.TemplTypes where


type ID = String

data Templ = RawTempl String
           | EvalTempl ID (IO String)

templID :: Templ -> ID
templID (EvalTempl i _) = i
templID _ = ""




