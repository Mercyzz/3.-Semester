theory InsertionSort
imports Main
begin

  fun insert :: "nat \<Rightarrow> nat list \<Rightarrow> nat list" where
    "insert x []      = [x]" |
    "insert x (y#ys)  = (if x \<le> y then x # y # ys else y # insert x ys)" 
  
  fun isort :: "nat list \<Rightarrow> nat list" where
    "isort []         = []" |
    "isort (x#xs)     = insert x (isort xs)"

  fun lower :: "nat \<Rightarrow> nat list \<Rightarrow> bool" where
    "lower n []       = True" |
    "lower n (x#xs)   = (n \<le> x \<and> lower n xs)"

  fun sorted :: "nat list \<Rightarrow> bool" where
    "sorted []        = True" |
    "sorted (x#xs)    = (lower x xs \<and> sorted xs)"
  

  lemma le_lower : "m \<le> n \<Longrightarrow> (lower n xs \<Longrightarrow> lower m xs)" 
    proof (induct xs arbitrary: m n)
      case Nil
        thus "?case" by simp
      next
      case (Cons x xs)
        assume asm1: "m \<le> n"
        assume asm2: "lower n (x#xs)"
        from asm2 have asm21: "n \<le> x" by simp
        from asm2 have asm22: "lower n xs" by simp
        from asm1 asm21 have res1: "m \<le> x" by simp
        from asm1 asm22 Cons.hyps have res2: "lower m xs" by simp
        from res1 res2 have "lower m (x#xs)" by simp
        thus "?case" by simp
      next
    qed

    lemma lowerInsert : "lower y (insert x as) = (y \<le> x \<and> lower y as)" 
    proof (induct as)
      case Nil
        thus "?case" by simp
        next
      case (Cons a as)
        thus "?case" 
        proof (cases "x \<le> a")
          case True
            then have "lower y (insert x (a#as)) = lower y (x#a#as)" by simp
            thus "?thesis" by simp
            next
          case False
            then have "lower y (insert x (a#as)) = lower y (a#(insert x as))" by simp
            also have "... = (y \<le> a \<and> lower y (insert x as))" by simp
            also have "... = (y \<le> a \<and> y \<le> x \<and> lower y as)" using Cons.hyps by simp
            also have "... = (y \<le> x \<and> lower y (a#as))" by (metis lower.simps(2))
            finally show "?thesis" by simp
            next
        qed
      qed

  lemma sortedInsert : "sorted ys = sorted (insert x ys)"
    proof (induct ys)
      case Nil
        have "sorted [] = True" by simp
        then have "... = sorted [x]" by simp
        thus "?case" by simp
      next
      case (Cons y ys)
        (*have "sorted (y#ys) = (lower y ys \<and> sorted ys)" by simp*)

        (*fix x*)
        let ?p = "x \<le> y"
        show "?case" using Cons.hyps
        proof (cases ?p) 
          case True 
          have "sorted (insert x (y#ys)) = sorted (x#y#ys)" by (simp add: `?p`)
          also have "... = (lower x (y#ys) \<and> sorted (y#ys))" by simp
          also have "... = ((x \<le> y \<and> lower x ys) \<and> (lower y ys \<and> sorted ys))" by simp
          also have "... = (lower y ys \<and> sorted ys)" by (metis `?p` le_lower)
          also have "... = sorted (y#ys)" by simp
          finally show "sorted (y#ys) = sorted (insert x (y#ys))" by simp
        next
        case False
          then have asm : "x > y" by simp
          then have asm2 : "y \<le> x" by simp
          have "sorted (insert x (y#ys)) = sorted (y#(insert x ys))" using asm by simp
          also have "... = (lower y (insert x ys) \<and> sorted (insert x ys))" by simp
          also have "... = (lower y (insert x ys) \<and> sorted ys)" using Cons.hyps by simp
          also have "... = (y \<le> x \<and> lower y ys \<and> sorted ys)" by (simp add: lowerInsert)
          also have "... = (lower y ys \<and> sorted ys)" by (simp add: asm2)
          also have "... = sorted (y#ys)" by simp
          finally show "?thesis" by simp
        next
      qed
      qed



          

(*           "x \<le> y \<Longrightarrow> sorted (y#ys) = sorted (insert x (y#ys))" by simp

*)
  lemma isortIsSorted : "sorted (isort xs)" 
    proof (induct xs)
      case Nil 
        thus "?case" by simp
      next
      case (Cons x xs)
        have "sorted (isort (x#xs)) = sorted (insert x (isort xs))" by simp
        then have "... = sorted (isort xs)" by (metis sortedInsert)
        thus "?case" by (metis Cons.hyps `sorted (isort (x # xs)) = sorted (insert x (isort xs))`)
      next
    qed

end
