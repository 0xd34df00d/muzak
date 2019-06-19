%default total

record DirEntry where
  constructor MkDirEntry

data Action : Type -> Type where
  ListDirectory : String -> Action (List DirEntry)
  RemoveFile    : String -> Action ()
  RemovePath    : String -> Action ()

data Script : Type -> Type where
  Pure : (val : res) -> Script res
  Bind : (act : Action r1) -> (step : r1 -> Script r2) -> Script r2

Functor Script where
  map f (Pure res) = Pure $ f res
  map f (Bind act step) = Bind act $ \x => f <$> step x

Applicative Script where
  pure = Pure
  (Pure f)        <*> script = f <$> script
  (Bind act step) <*> script = Bind act $ \x => step x <*> script

Monad Script where
  (Pure val)      >>= f = f val
  (Bind act step) >>= f = Bind act $ \x => step x >>= f
