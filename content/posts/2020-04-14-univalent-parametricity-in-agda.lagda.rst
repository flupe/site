---
title: Univalent Parametricity in Agda
draft: yes
---

.. highlight:: agda
.. default-role:: math

Assume you are proving properties on some object A. Moreover, assume that A is
known to be equivalent to some other object B. You ought to be able to apply the
properties you proved on A to B directly, as easily as you would in mathematics.
Likewise, any operation on A (think **computation**) ought to be "transformed"
into an "equivalent" operation on B automatically, for a good enough definition
of "equivalent". As it turns out, what ought to be simply isn't in proof
assistants. The proofs you write are tied to the representation you carry them
on.

In [Tabareau2019]_, they show how both univalence and parametricity are not quite
enough to give us what we are looking for. But they demonstrate that with
**univalent parametricity** you are able to get what you ought to have.
After failing to make a good enough implementation of it in Coq during an internship,
here is me trying to make one in Cubical Agda.

This post is following the literate programming tradition, using Agda 2.6.1.

.. [Tabareau2019] The Marriage of Univalence and Parametricity, 2019
                  (Nicolas Tabareau, Eric Tanter, Matthieu Sozeau)

Setting things up
~~~~~~~~~~~~~~~~~

.. raw:: html

  <details>
    <summary>Imports</summary>

::

  {-# OPTIONS --cubical --without-K --safe #-}

  open import Agda.Primitive
  open import Agda.Builtin.Sigma

  open import Data.List.Base
  open import Cubical.Data.Sigma.Properties

  open import Cubical.Foundations.Function using (_∘_; const)
  open import Cubical.Foundations.Prelude hiding (Type)
  open import Cubical.Foundations.Isomorphism
  open import Cubical.Foundations.Equiv
  open import Cubical.Foundations.HLevels
  open import Cubical.Foundations.Equiv.HalfAdjoint using (congEquiv)
  open import Cubical.Foundations.Equiv.Properties
  open import Cubical.Foundations.Univalence

  variable i j : Level

  infixr 1 _$_
  _$_ : {A : Set i} {P : A → Set j} (f : ∀ x → P x) (x : A) → P x
  f $ x = f x

  id : {A : Set i} → A → A
  id x = x

  _×_ : (A B : Set i) → Set i
  A × B = Σ A (const B)

.. raw:: html

  </details>


We begin by specifying the type of binary relations, since those are what we
are going to manipulate from now on.

::

  _∼_ : (A B : Set i) → Set (lsuc i)
  _∼_ {i} A B = A → B → Set i

  ∼-inv : {A B : Set i} → A ∼ B → B ∼ A
  ∼-inv R = λ b a → R a b

  ∼-comp : {A B C : Set i} → A ∼ B → B ∼ C → A ∼ C
  ∼-comp RAB RBC = λ a c → Σ _ λ b → RAB a b × RBC b c

Then we introduce our **type-indexed univalent logical relation**.

.. math:: {·⋈·} : {(A : \textsf{Set}_i) (B : \textsf{Set}_i) → \text{Rel}\ A\ B}

We use the notation `x≈y⟨A ⋈ B⟩` to denote `(x , y) ∈ (A ⋈ B)`. The shorthand `x≈y`
will appear when the relation is obvious.


::

  record _⋈_ (A B : Set i) : Set (lsuc i) where
    constructor UR
    field ur : A ∼ B
  open _⋈_

  ⋈-refl : (A : Set i) → A ⋈ A
  ⋈-refl _ = UR _≡_

  ⋈-sym : {A B : Set i} → A ⋈ B → B ⋈ A
  ⋈-sym RAB = UR (∼-inv (ur RAB))

  ⋈-trans : {A B C : Set i} → A ⋈ B → B ⋈ C → A ⋈ C
  ⋈-trans {B = B} RAB RBC = UR (∼-comp (ur RAB) (ur RBC))

We might be tempted to define an infix operator :code:`_≈_` to relate terms of type A and B,
provided the logical relation exists at A and B. However, after experimenting a bit with such an idea,
I found the instance search mechanism of Agda to be too limited. More on that later.
This also means we will not get the nice syntax of the initial Coq implementation for free.

We want :code:`⋈` to satisfy some constraints as it cannot be any relation::

  isFun : {A B : Set i} (R : A ∼ B) → Set i
  isFun {A = A} {B} R = (a : A) → isContr (Σ[ b ∈ B ] R a b)

  record isBifun {A B : Set i} (R : A ∼ B) : Set i where
    constructor _,_
    field
      fun    : isFun R
      funInv : isFun (∼-inv R)
  open isBifun

  funR   : {A B : Set i} {R : A ∼ B} (F : isFun R) → A → B
  center : {A B : Set i} {R : A ∼ B} (F : isFun R) → (x : A) → R x (funR F x)
  contr₁ : {A B : Set i} {R : A ∼ B} (F : isFun R) → ∀ {x} {y} → R x y → funR F x ≡ y
  contr₂ : {A B : Set i} {R : A ∼ B} (F : isFun R) → ∀ {x} {y} (r : R x y)
         → PathP (λ i → R x (contr₁ F r i)) (center F x) r
  contr₂ f {x} {y} r = λ i → {! snd (snd (f x) (y , r) i) !}

  isFunIsProp    : {A B : Set i} (R : A ∼ B) → isProp (isFun R)
  isFunInvIsProp : {A B : Set i} (R : A ∼ B) → isProp (isFun (∼-inv R))

  isBifun⇒isEquiv : {A B : Set i} {R : A ∼ B} (F : isBifun R) → isEquiv (funR (fun F))
  isBifun⇒≃ : {A B : Set i} {R : A ∼ B} (F : isBifun R) → A ≃ B

  isFunRf : {A B : Set i} (f : A → B) → isFun (λ a b → f a ≡ b)
  isFunRf f = λ a → (f a , refl)
            , λ where (b , e) → ΣPathP (e , (λ i₁ i₂ → e (i₁ ∧ i₂)))

  isFunRfInvisBifun : {A B : Set i} (f : A -> B) →
                      isFun (λ b a → f a ≡ b) → isBifun (λ a b → f a ≡ b)
  isFunRfInvisBifun f fun .fun    = isFunRf f
  isFunRfInvisBifun f fun .funInv = fun

  isFun-≡ : (A : Set i) → isFun (_≡_ {A = A})
  isFun-≡ A = isFunRf id

  isFun-equiv : {A B : Set i} {R R′ : A ∼ B} →
                (∀ x y → R x y ≃ R′ x y) → isFun R → isFun R′
  isFun-equiv e f a =
    ( funR f a , fst (e a (funR f a)) (center f a))
    , λ where (b , a≈b) → let z = contr₁ f (invEq (e a b) a≈b)
                              v = contr₂ f (invEq (e a b) a≈b) in
                          ΣPathP (z , {!!})

  isFun-comp : {A B C : Set i} {RAB : A ∼ B} {RBC : B ∼ C} →
               isFun RAB → isFun RBC → isFun (∼-comp RAB RBC)

  isBifun-≡ : (A : Set i) → isBifun (_≡_ {A = A})
  isBifun-≡ A = isFun-≡ A
           , isFun-equiv (λ x y → sym , record { equiv-proof = λ y≡x → (sym y≡x , refl)
                                                             , λ y₁ i₁ → {!!} }) (isFun-≡ A)

  isBifun-inv     : {A B : Set i} {R : A ∼ B} → isBifun R → isBifun (∼-inv R)

  isBifun-comp : {A B C : Set i} {RAB : A ∼ B} {RBC : B ∼ C} →
               isBifun RAB → isBifun RBC → isBifun (∼-comp RAB RBC)

  isBifun-comp FAB FBC = {!!}

..
  ::
  funR f   = fst ∘ fst ∘ f
  center f = snd ∘ fst ∘ f
  contr₁ f {x} {y} r = cong fst $ snd (f x) (y , r)

  isFun-comp {RBC = RBC} fab fbc a
    = (c  , b  , Rab  , Rbc  ) , λ where
      (c′ , b′ , Rab′ , Rb′c′) → {!!}
        -- let z : (b , Rab) ≡ (b′ , Rab′)
        --     z = (snd (fab a) (b′ , Rab′))
        --     b≡b′ : b ≡ b′
        --     b≡b′ = cong fst z
        --     w : (c , Rbc) ≡ (c′ , {!!})
        --     w = (snd (fbc b) (c′ , {!!}))
        -- in ΣPathP ({!!} , λ i₁ → (b≡b′ i₁) , {!!} , {!!})
    where
      b   = funR fab a
      Rab = center fab a
      c   = funR fbc b
      Rbc = center fbc b

  isFunIsProp R = λ f g → funExt (λ a → isPropIsContr (f a) (g a))

  isFunInvIsProp R = isFunIsProp (∼-inv R)

  isBifun-inv b .fun    = funInv b
  isBifun-inv b .funInv = fun b

  isBifun⇒≃ (f , g) = isoToEquiv $ iso (funR f) (funR g)
    (λ b → cong fst (snd (f (funR g b)) (b , center g b)))
    (λ a → cong fst (snd (g (funR f a)) (a , center f a)))

  isBifun⇒isEquiv F = snd $ isBifun⇒≃ F


The original paper then defines a univalent logical relation at every type constructor of CIC.
We are working in Agda so we are morally using the type constructors of ITT (I think?),
but they are identical in practice.

We start with the relation at Set::

  record ≈-Set (A B : Set i) : Set (lsuc i) where
    field
      rel   : A ⋈ B
      bifun : isBifun (ur rel)
  open ≈-Set

  ⋈-Set : Set i ⋈ Set i
  ⋈-Set = UR ≈-Set

Transport
~~~~~~~~~

Assuming two types are related at Set, we can lift from one type to another::

  □_ : {A B : Set i} ⦃ R : ur ⋈-Set A B ⦄ → A → B
  □_ ⦃ R ⦄ = funR (R .bifun .fun)

This is not very interesting per se: from :code:`R : A ≈ B` we can
retrieve an equivalence from A to B
The interesting part will be to construct this :code:`R : A ≈ B` automatically.

Dependent products
~~~~~~~~~~~~~~~~~~

Given `A, B : \textsf{Set}_i` such that `A ⋈ B` is defined,
we define the relation `A → \textsf{Set}_i ⋈ B → \textsf{Set}_i` as:

.. math::

  P ≈ Q⟨A → \textsf{Set}_i ⋈ B → \textsf{Set}_i⟩
    \quad\iff\quad
  ∀(x : A)(y : B), x ≈ y⟨A ⋈ B⟩ ⇒ {P\ x} ≈ {Q\ y}⟨\textsf{Set}_i ⋈ \textsf{Set}_i⟩

Given:

- `A, B : \textsf{Set}_i` such that `A ⋈ B` is defined;
- `P : A → \textsf{Set}_i`, `Q : B → \textsf{Set}_i`
  such that `P ≈ Q⟨A → \textsf{Set}_i ⋈ B → \textsf{Set}_i⟩`;
  
we define the relation `{Πx:A.P\ x ⋈ Πy:B.Q\ y}` as:

.. math::

   f ≈ g ⟨ Πx:A.P\ x ⋈ Πy:B.Q\ y ⟩ \quad\iff\quad 
   ∀ (x : A)(y : B), x ≈ y⟨A ⋈ B⟩ ⇒ {f\ x} ≈ {f\ y}⟨P\ x ⋈ Q\ y ⟩

::

  ⋈-∀′ : {A B : Set i} → A ⋈ B → (A → Set i) ⋈ (B → Set i)
  ⋈-∀′ R = UR λ P Q → ∀ {x y} → ur R x y → ur ⋈-Set (P x) (Q y)

  ⋈-∀ : {A B : Set i} (R : A ⋈ B) →
        {P : A → Set i} {Q : B → Set i} → ur (⋈-∀′ R) P Q
      → (∀ x → P x) ⋈ (∀ y → Q y)
  ⋈-∀ RA RF = UR λ f g → ∀ {x y} (H : ur RA x y) → ur (RF H .rel) (f x) (g y)

We prove a few properties "just for fun"::

  ≈-refl  : (A     : Set i) → ur ⋈-Set A A
  ≈-sym   : {A B   : Set i} → ur ⋈-Set A B → ur ⋈-Set B A
  ≈-trans : {A B C : Set i} → ur ⋈-Set A B → ur ⋈-Set B C → ur ⋈-Set A C

..
  ::

  ≈-refl A = record
    { rel   = ⋈-refl A
    ; bifun = isBifun-≡ A
    }

  ≈-sym RAB = record
    { rel   = ⋈-sym (rel RAB)
    ; bifun = isBifun-inv (bifun RAB)
    }

  ≈-trans RAB RBC = record
    { rel   = ⋈-trans (rel RAB) (rel RBC)
    ; bifun = isBifun-comp (bifun RAB) (bifun RBC)
    }

Type Constructors are Univalently Parametric
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We cannot prove the abstraction theorem inside the theory itself, that is:
every term is univalently related to itself. What we can do however,
is prove that every type constructor is univalently parametric.

.. math:: \textsf{Set}_i ≈ \textsf{Set}_i ⟨ \textsf{Set}_{i+1} ⋈ \textsf{Set}_{i+1} ⟩


Set
---

First we prove the fixpoint property of the univalent relation at Set::

  Set≈Set : ur ⋈-Set (Set i) (Set i)
  Set≈Set {i} = record
    { rel   = ⋈-Set
    ; bifun = record
        { fun = λ A →
            (A , ≈-refl A)
            , λ where (B , A≈B) →
                        let A≡B = ua (isBifun⇒≃ (bifun A≈B))
                        in ΣPathP (A≡B , λ i → {!!})
        ; funInv = {!!}
        }
    }

Dependent Function Type
-----------------------

.. math::

   \frac{{{A : \textsf{Set}_i \qquad B : \textsf{Set}_i \qquad A ≈ B ⟨ \textsf{Set}_i ⋈ \textsf{Set}_i⟩}
     \atop
     {P : A → \textsf{Set}_i \qquad Q : B → \textsf{Set}_i \qquad P ≈ Q ⟨A → \textsf{Set}_i ⋈ B → \textsf{Set}_i⟩}}
   }{
     ∀ x → P\ x ≈ ∀ y → Q\ y ⟨\textsf{Set}_i ⋈ \textsf{Set}_i⟩
   }
::

  ∀≈∀ : {A B : Set i} (RA : ur ⋈-Set A B) →
        {P : A → Set i} {Q : B → Set i} (RF : ur (⋈-∀′ (rel RA)) P Q)
      → ur ⋈-Set (∀ x → P x) (∀ y → Q y)
  ∀≈∀ RA RF = record
    { rel   = ⋈-∀ (rel RA) RF
    ; bifun = record
        { fun = λ f → (( λ b → funR (RF (center inv b) .bifun .fun) (f (funR inv b)))
                       , λ {x} {y} H → {!funR (RF H .bifun .fun) (f x)!})
                      , λ where (q , f≈q) → ΣPathP
                                  (funExt (λ x → {!!}) , {!!})
        ; funInv = λ g → {!!}
        }
    } where inv = RA .bifun .funInv

Univalent relation for parametrized datatypes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When we declare a parametrized datatype `F : Δ → \textsf{Set}_i`, we introduce a new type constructor
inside the theory. Therefore we need to extend the univalent logical relation at this type former.
That is, given `x, y : Δ` such that `x ≈ y ⟨ Δ ⋈ Δ⟩`, we need to define `{F\ x ⋈ F\ y}`.

Let's look at a concrete exemple first.

The case of lists
-----------------

Recall the definition of inductive lists:

.. code-block:: agda

  data List (A : Set i) : Set i where
    []  : List A
    _∷_ : A → List A → List A

Our goal is to define, for `A, B : \textsf{Set}_i` such that `A ⋈ B` is defined, the relation
`{\textsf{List}\ A} ⋈ {\textsf{List}\ B}`. It seems reasonable to say that two lists
`xs` and `ys` are related iff they are the same length and their elements are related one-to-one.

We can define this relation inductively:

- `[] ≈ []`
- For any `x : A`, `y : B`, `xs : \textsf{List}\ A`, `ys : \textsf{List}\ B`
  such that `x ≈ y` and `xs ≈ ys`, then `(x ∷ xs) ≈ (y ∷ ys)`

::

  data ≈-List {A B : Set i} (R : A ⋈ B) : List A ∼ List B where
    ≈-[] : ≈-List R [] []
    ≈-∷  : ∀ {x y xs ys} → ur R x y → ≈-List R xs ys → ≈-List R (x ∷ xs) (y ∷ ys)

  instance
    ⋈-List : {A B : Set i} ⦃ R : A ⋈ B ⦄ → List A ⋈ List B
    ⋈-List ⦃ R ⦄ = UR (≈-List R)

Now we have to prove that `\textsf{List}` is univalently parametric.

::

  List≈List : {A B : Set i} ⦃ R : ur ⋈-Set A B ⦄ → ur ⋈-Set (List A) (List B)
  List≈List {A = A} {B} ⦃ R ⦄ = record
    { rel   = ⋈-List ⦃ rel R ⦄
    ; bifun = (λ xs → (map (funR f) xs , xs≈fxs xs) , contr-R xs)
            , (λ ys → (map (funR g) ys , gys≈ys ys) , contr-G ys)
    }
    where
      f = R .bifun .fun
      g = R .bifun .funInv

      xs≈fxs : ∀ xs → ≈-List (rel R) xs (map (funR f) xs)
      xs≈fxs []       = ≈-[]
      xs≈fxs (x ∷ xs) = ≈-∷ (center f x) (xs≈fxs xs)

      contr-R : ∀ xs (y : Σ (List B) (≈-List (rel R) xs)) → (map (funR f) xs , xs≈fxs xs) ≡ y
      contr-R [] (.[] , ≈-[]) = refl
      contr-R (x ∷ xs) (y ∷ ys , ≈-∷ x≈y xs≈ys) = ΣPathP
        ( cong₂ _∷_ (contr₁ f x≈y)   (cong fst (contr-R xs (ys , xs≈ys)))
        , λ i → ≈-∷ (contr₂ f x≈y i) (cong snd (contr-R xs (ys , xs≈ys)) i)
        )

      gys≈ys : ∀ ys → ≈-List (rel R) (map (funR g) ys) ys
      gys≈ys []       = ≈-[]
      gys≈ys (y ∷ ys) = ≈-∷ (center g y) (gys≈ys ys)

      contr-G : ∀ ys (p : Σ (List A) (λ xs → ≈-List (rel R) xs ys))
              → (map (funR g) ys , gys≈ys ys) ≡ p
      contr-G [] (.[] , ≈-[]) = refl
      contr-G (y ∷ ys) (x ∷ xs , ≈-∷ x≈y xs≈ys) = ΣPathP
        ( cong₂ _∷_ (contr₁ g x≈y)   (cong fst (contr-G ys (xs , xs≈ys)))
        , λ i → ≈-∷ (contr₂ g x≈y i) (cong snd (contr-G ys (xs , xs≈ys)) i)
        )

A bit of reflection magic
~~~~~~~~~~~~~~~~~~~~~~~~~

Ok, now the harsh truth: Agda's instance search is very limited.
In particular, you cannot have two overlapping instances in scope.
In the original paper, proof search is doing the heavy work.

Let's try to make this with a tiny (tiny) bit of reflection.

::

  open import Agda.Builtin.Unit
  open import Agda.Builtin.Nat
  open import Agda.Builtin.Reflection
  open import Reflection.TypeChecking.Monad.Syntax

  build-ur : Type → Type → TC Term
  -- hardcoded shit
  build-ur (agda-sort (lit 0)) (agda-sort (lit 0)) = do
    returnTC (def (quote Set≈Set) (arg (arg-info hidden relevant) (quoteTerm lzero) ∷ []))

  build-ur (lam _ _) (lam _ _) = do
    {!!}

  build-ur (pi (arg (arg-info v₁ _) A) P) (pi (arg (arg-info v₂ _) B) Q) = do
    A≈B ← build-ur A B
    P≈Q ← build-ur (lam v₁ P) (lam v₂ Q)
    returnTC (def (quote ∀≈∀) ( arg (arg-info visible relevant) A≈B
                              ∷ arg (arg-info visible relevant) P≈Q
                              ∷ []))

  build-ur s t = typeError ( strErr "Unable to match types"
                           ∷ termErr s
                           ∷ strErr "and"
                           ∷ termErr t
                           ∷ [])

  macro
    ok : Name → Name → Term → TC ⊤
    ok left right hole = do
      A   ← getType left
      B   ← getType right
      A≈B ← build-ur A B
      unify hole (def (quote ur) ( arg (arg-info visible relevant)
                                       (def (quote rel) (arg (arg-info visible relevant) A≈B ∷ []))
                                 ∷ arg (arg-info visible relevant) (def left [])
                                 ∷ arg (arg-info visible relevant) (def right [])
                                 ∷ []))

  postulate f : Set → Set
..

  Ah, if only one had datatype-generic programming
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  Ok, that's nice. But if you were to take a look at the proofs of the previous properties,
  you will soon realise that although they are tedious, they have a very predictable structure.
  By that I mean that there is nothing specific about lists in these proofs, and that they ought
  to be derived for any datatype, rather than handwritten.
  
  Now let's see how to derive the univalent logical relation for
  user-defined datatypes. When implementing this in Coq, I had to rely on
  MetaCoq_, and more specifically TemplateCoq_.
  In Agda however, we have reflection built-in! There is are two key differences
  that will make my life easier:
  
  - universe levels are **first-class values**;
  - we can leave **holes** in term that we are defining.
    This is especially useful because it allows us to be less explicit
    about universe levels and implicit function arguments.

.. _MetaCoq: https://github.com/MetaCoq/metacoq
.. _TemplateCoq: https://github.com/MetaCoq/metacoq
