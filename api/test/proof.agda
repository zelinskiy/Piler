
open import Agda.Primitive


infix 4 _≡_

data _≡_ {a} {A : Set a} (x : A) : A → Set a where
  refl : x ≡ x

{-# BUILTIN EQUALITY _≡_ #-}
{-# BUILTIN REFL refl #-}

postulate trustMe : ∀ {a} {A : Set a} {x y : A} → x ≡ y

data Singleton {a} {A : Set a} (x : A) : Set a where
  _with≡_ : (y : A) → x ≡ y → Singleton x

inspect : ∀ {a} {A : Set a} (x : A) → Singleton x
inspect x = x with≡ refl

cong :  ∀ {ℓ} {A : Set ℓ} → {a b : A} → {B : Set ℓ} → (f : A → B) →
  a ≡ b → (f a) ≡ (f b)
cong f refl = refl

sym : ∀ {ℓ} {A : Set ℓ} → {a b : A} → a ≡ b → b ≡ a
sym refl = refl

data Bool : Set where
  true false : Bool

if_then_else_ : ∀ {ℓ} {A : Set ℓ} → Bool → A → A → A
if true then a₁ else a₂ = a₁
if false then a₁ else a₂ = a₂

id : ∀ {ℓ} {A : Set ℓ} → A → A
id = λ x → x

const : ∀ {ℓ₁ ℓ₂} {A : Set ℓ₁} {B : Set ℓ₂} → A → B → A
const a _ = a

infixl 30 _∘_
_∘_ : ∀ {ℓ} {A : Set ℓ}{B : A → Set ℓ}{C : {x : A} → B x → Set ℓ}
    → (f : {x : A} → (y : B x) → C y) → (g : (x : A) → B x) → ((x : A) → C (g x))
f ∘ g = λ x → f (g x)

record Functor {ℓ₁ ℓ₂} (F : Set ℓ₁ → Set ℓ₂) : Set (lsuc ℓ₁ ⊔ ℓ₂) where
  infixl 4 _<$>_ 
  field
    fmap : ∀ {A B} → (A → B) → F A → F B
    law1 : ∀ {A} → (func : F A) → fmap id func ≡ id func
    law2 : ∀ {A B C} → (f : B → C) → (g : A → B) → (func : F A) →
      fmap (f ∘ g) func ≡ ((fmap f) ∘ (fmap g)) func
  _<$>_ = fmap

open Functor {{...}} public

record Monad {ℓ₁ ℓ₂} (M : Set ℓ₁ → Set ℓ₂) : Set (lsuc ℓ₁ ⊔ ℓ₂) where
  field
    return : ∀ {A} → A → M A
    _>>=_ : ∀ {A B} → M A → (A → M B) → M B
    lidentity : ∀ {A B} → (a : A) (f : A → M B) → (return a) >>= f ≡ f a
    ridentity : ∀ {A} → (m : M A) → m >>= return ≡ m
    assoc : ∀ {A B C} → (m : M A) (f : A → M B) (g : B → M C) →
      (m >>= f) >>= g ≡ m >>= (λ x → (f x >>= g))

open Monad {{...}} public

postulate lift : {m : Set → Set₁} {t : (Set → Set₁) → Set → Set₁} {a : Set} → m a -> t m a

data ReaderT (r : Set) (m : Set → Set₁) (a : Set) : Set₁ where
  mkReaderT : (r -> m a) → ReaderT r m a

runReaderT : ∀ {r m a} → ReaderT r m a → r → m a
runReaderT (mkReaderT f) = f

instance
  monadReaderT : (r : Set) → (m : Set → Set₁) → {{im : Monad m}} → Monad (ReaderT r m)
  monadReaderT r m {{im}}  = record
                       { return = λ x → mkReaderT (λ y → return x)
                       ; _>>=_ = λ m k → mkReaderT (λ r → runReaderT m r >>= λ a → runReaderT (k a) r)
                       ; lidentity = λ a f → trustMe
                       ; ridentity = λ m₁ → trustMe
                       ; assoc = λ m₁ f g → trustMe
                       }

postulate
  Entity : Set → Set
  instance functorReaderT : (r : Set) → (m : Set → Set₁) → Functor (ReaderT r m)
  instance functorMonad : ∀ {m : Set → Set₁}{{_ : Monad m}} → Functor m
  IO : Set → Set₁
  instance monadIO : Monad IO
  ConnectionPool User UserStatus : Set
  userStatus : User → UserStatus
  entityVal : ∀ {e} → Entity e → e
  ExceptT : Set → (Set → Set₁) → Set → Set₁
  throwError : ∀ {e a : Set} {m : Set → Set₁} {{_ : Monad m }} → e → m a
  Error : Set
  err401 : Error
  

ask : {r : Set} {m : Set → Set₁} {{_ : Monad m}} → ReaderT r m r
ask = mkReaderT return

Handler = ExceptT Error IO
PublicHandler = ReaderT ConnectionPool Handler 
PrivateHandler = ReaderT (Entity User) PublicHandler

postulate
  instance functorPrivateHandler : Functor PrivateHandler
  instance monadPrivateHandler : Monad PrivateHandler
  instance monadHandler : Monad Handler
  instance monadPublicHandler : Monad PublicHandler
  throwErrorAnything : ∀ {e a : Set} {m : Set → Set₁}
    {{_ : Monad m }} {h : m a} → throwError err401 ≡ h
  askAndIgnore : ∀ {a : Set} → (h : PrivateHandler a) →
    ask >>= const h ≡ h  

data Natural {A : Set}
  (F G : Set → Set₁)
  {{if₁ : Functor F}}
  {{if₂ : Functor G}} : Set₁ where
  NT : (F A → G A) → Natural F G
  
ver : ∀ {A : Set} {F G H : Set → Set₁} {{i₁ : Functor F}}
  {{i2 : Functor G}} {{i3 : Functor H}} →
  Natural {A} F G → Natural {A} G H → Natural {A} F H
ver (NT eta) (NT eps) = NT (λ x → eps (eta x))

checkRole : ∀ {A : Set} → (UserStatus → Bool) →
  Natural {A} PrivateHandler PrivateHandler
  {{functorPrivateHandler}} {{functorPrivateHandler}}
checkRole {A} p = NT (λ h → ask
  >>= (λ r → if (p (userStatus (entityVal r))) then h else throwError err401))

data Entered {a : Set} (m n : Set → Set₁) {{if₁ : Functor m}} {{if₂ : Functor n}}
  (t : (Set → Set₁) → Set → Set₁) : Set₁ where

postulate  
  enter : ∀ {a : Set} {m n : Set → Set₁} {{if₁ : Functor m}}
    {{if₂ : Functor n}} {t : (Set → Set₁) → Set → Set₁} →
    (Natural {a} n m) → Entered {a} n m t → t m a
  Extensionality :
    ∀ {a b} {A : Set a} {B : A → Set b} {f g : (x : A) → B x} →
    (∀ x → f x ≡ g x) → f ≡ g


lemma1 : ∀ {a : Set} {p : UserStatus → Bool} {u : Entity User}
  {h : PrivateHandler a} →
   if p (userStatus (entityVal u))
   then ask >>= (λ x → h)
   else throwError err401
   ≡ h
lemma1 {a}{p}{u}{h} with p (userStatus (entityVal u)) 
... | true = askAndIgnore h
... | false = throwErrorAnything {e = Error}

lemma2 : ∀ {a : Set} {p : UserStatus → Bool} {u : Entity User}
  {h : PrivateHandler a} → 
    (if p (userStatus (entityVal u)) then
       ask >>= (λ r → if p (userStatus (entityVal r))
                      then h
                        else throwError err401)
                      else throwError err401)
      ≡ (if p (userStatus (entityVal u)) then h else throwError err401)
lemma2 {a} {p} {u} {h} with p (userStatus (entityVal u))
... | true rewrite sym (askAndIgnore h) =
  cong (Monad._>>=_ monadPrivateHandler ask)
       (Extensionality (λ x → lemma1{p = p}))
... | false = refl

checkRoleIdempotent : ∀ {a : Set} {p : UserStatus → Bool}
  {t : (Set → Set₁) → Set → Set₁}
  {e : Entered {a = a} PrivateHandler PrivateHandler
    {{functorPrivateHandler}} {{functorPrivateHandler}} t } →
  enter {a = a} {t = t} (checkRole p)
    ≡ enter (ver (checkRole p) (checkRole p))
checkRoleIdempotent {p = p} =
  cong enter
    (cong NT
      (sym
        (Extensionality
          (λ h → cong
            (Monad._>>=_ monadPrivateHandler ask)
              (Extensionality
                (λ u →
                  lemma2{p = p}{u}{h}))))))
