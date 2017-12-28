open import Agda.Primitive

infix 4 _≡_

data _≡_ {a} {A : Set a} (x : A) : A → Set a where
  refl : x ≡ x

{-# BUILTIN EQUALITY _≡_ #-}
{-# BUILTIN REFL refl #-}

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

postulate
  ReaderT : Set → (Set → Set₁) → Set → Set₁
  runReaderT : ∀ {r m a} → ReaderT r m a → r → m a
  ask : {r : Set} {m : Set → Set₁} {{_ : Monad m}} → ReaderT r m r
  Entity : Set → Set
  instance functorReaderT : (r : Set) → (m : Set → Set₁) → Functor (ReaderT r m)
  instance monadReaderT : (r : Set) → (m : Set → Set₁) → Monad (ReaderT r m)
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
 
Handler = ExceptT Error IO
PublicHandler = ReaderT ConnectionPool Handler 
PrivateHandler = ReaderT (Entity User) PublicHandler

postulate
  instance functorPrivateHandler : Functor PrivateHandler
  instance monadPrivateHandler : Monad PrivateHandler
  instance monadHandler : Monad Handler
  instance monadPublicHandler : Monad PublicHandler
  

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

-- Follows directly from the actual return definition (return a ≡ \r -> a)
postulate lemma1 : ∀ {a : Set} →
            (h : PrivateHandler a) →
            ask >>= const h ≡ h

lemma2 : ∀ {a : Set} → (h : PrivateHandler a) →
  (monadPrivateHandler Monad.>>= ask)
    (λ r → (monadPrivateHandler Monad.>>= ask) (λ r₁ → h))
  ≡ (monadPrivateHandler Monad.>>= ask) (λ r → h)
lemma2 h rewrite lemma1 h | lemma1 h = refl

checkRoleIdempotent : ∀ {a : Set} {p : Bool}
  {t : (Set → Set₁) → Set → Set₁}
  {e : Entered {a = a} PrivateHandler PrivateHandler
    {{functorPrivateHandler}} {{functorPrivateHandler}} t } →
  enter {a = a} {t = t} (checkRole (const p))
    ≡ enter (ver (checkRole (const p)) (checkRole (const p)))
checkRoleIdempotent {p = true} =
  cong enter (cong NT (sym (Extensionality lemma2)))
checkRoleIdempotent {p = false} =
  cong enter (cong NT refl)
