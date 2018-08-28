;; Count how many files cannot be parsed by CDT

(ns atom-finder.questions.count-parse-failures
  (:require
   [clj-cdt.clj-cdt :refer :all]
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.questions.question-util :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]
   [schema.core :as s]
   [swiss.arrows :refer :all]
   )
  )

;; how many files do not parse at all
'((do
    (prn (now))
    (->>
     atom-finder-corpus-path
     ;;"~/opt/src/atom-finder/gcc/gcc/testsuite/" expand-home
     c-files
     (map #(->> % parse-file filename (log-err (.getAbsolutePath %) nil)))
     doall
     (def parsed-files)
     time-mins
     )


    (->> parsed-files (take 2))
    (->> parsed-files count prn)
    (->> parsed-files (remove nil?) count prn)
    (->> parsed-files (filter nil?) count prn)
    ))

;; rate of problem nodes
;; takes ~1hour on hepheastus
'((do
    (prn (now))
    (->>
     atom-finder-corpus-path
     c-files
     ;; (take 1000)
     (map #(->> % parse-file flatten-tree (frequencies-by typename)
                (log-err (.getAbsolutePath %) nil)))
     (reduce (partial merge-with +))
     (def node-type-count)
     pprint
     time-mins
     )
    ))

'((do
    (def node-type-count
      {"ExplicitTemplateInstantiation" 5777,
       "UnaryExpression" 10057612,
       "TemplateSpecialization" 13756,
       "DesignatedInitializer" 832818,
       "BaseSpecifier" 66917,
       "ASMDeclaration" 20404,
       "TemplatedTypeTemplateParameter" 1383,
       "NamespaceDefinition" 82411,
       "BreakStatement" 493628,
       "TryBlockStatement" 2831,
       "CompositeTypeSpecifier" 320130,
       "ReferenceOperator" 670159,
       "LambdaExpression" 10713,
       "DoStatement" 111001,
       "ArrayDeclarator" 381201,
       "TypeIdInitializerExpression" 7218,
       "ConditionalExpression" 284278,
       "SimpleTypeConstructorExpression" 11295,
       "FunctionWithTryBlock" 93,
       "RangeBasedForStatement" 26263,
       "ElaboratedTypeSpecifier" 2016285,
       "ArrayModifier" 391350,
       "FieldReference" 9394159,
       "NewExpression" 51648,
       "LabelStatement" 111830,
       "TemplateDeclaration" 107923,
       "CatchHandler" 4170,
       "InitializerList" 1686655,
       "IfStatement" 3138057,
       "UsingDeclaration" 23234,
       "Pointer" 4728894,
       "CastExpression" 999363,
       "FieldDeclarator" 97373,
       "ParameterDeclaration" 5160799,
       "TypeId" 1913510,
       "Enumerator" 476616,
       "ConstructorChainInitializer" 188051,
       "CaseStatement" 835786,
       "PointerToMember" 4695,
       "CompoundStatementExpression" 12976,
       "NamespaceAlias" 379,
       "SimpleDeclaration" 6665997,
       "Problem" 828852,
       "TypeIdExpression" 166155,
       "DeleteExpression" 13714,
       "SimpleDeclSpecifier" 6054439,
       "GotoStatement" 261890,
       "ConstructorInitializer" 363233,
       "ProblemTypeId" 13,
       "ConversionName" 3684,
       "QualifiedName" 1873530,
       "UsingDirective" 15947,
       "TemplateId" 499954,
       "NamedTypeSpecifier" 7186779,
       "VisibilityLabel" 119514,
       "WhileStatement" 96130,
       "BinaryExpression" 14534213,
       "StaticAssertionDeclaration" 13770,
       "EqualsInitializer" 2009476,
       "FunctionDefinition" 1881467,
       "ProblemDeclaration" 600287,
       "ExpressionStatement" 9569398,
       "OperatorName" 44129,
       "FunctionDeclarator" 2913433,
       "SimpleTypeTemplateParameter" 197082,
       "Capture" 5553,
       "NullStatement" 137481,
       "DeclarationStatement" 3187750,
       "ProblemExpression" 5951,
       "Declarator" 12462059,
       "ProblemStatement" 222601,
       "AliasDeclaration" 9249,
       "DefaultStatement" 83592,
       "Name" 79294274,
       "FunctionCallExpression" 10564840,
       "SwitchStatement" 111838,
       "EnumerationSpecifier" 57285,
       "CompoundStatement" 4364820,
       "ReturnStatement" 2339629,
       "ContinueStatement" 77295,
       "TranslationUnit" 220787,
       "ForStatement" 308452,
       "PackExpansionExpression" 2726,
       "IdExpression" 42391631,
       "LiteralExpression" 29684279,
       "ArraySubscriptExpression" 1420466,
       "LinkageSpecification" 7932,
       "ExpressionList" 81652})

    (->> node-type-count
         (separate (%->> first (re-find #"Problem")))
         (map #(sum (map last %)))
         (apply /)
         float
         prn) ;; => 0.0057953335
    )
  )


;; csv of all files and what ratio of problem nodes they contain
;; used to identify a threshold for which files are "parsable" and
;; which aren't. Useful for excluding things like ChangeLogs, etc.
'((->> "~/opt/src/atom-finder/"
       expand-home
       files-in-dir
       ;(take 100)
       (drop-while #(not= "/home/dgopstein/opt/src/atom-finder/gcc/gcc/testsuite/gcc.c-torture/compile/limits-structnest.c" (.getPath %)))
       (map (partial pap (memfn getPath)))
       (map (fn [file]
              (with-timeout 10
              (log-err "couldn't parse" nil
              (let [h (->> file
                   parse-file
                   flatten-tree
                   (group-by problem?)
                   (map-values count))

                    total-nodes (+ (get h true 0) (get h false 0))
                    problem-rate (float (/ (get h true 0)
                                         total-nodes))
                    file-path (str/replace (.getPath file) (re-pattern (expand-home "~/opt/src/atom-finder/")) "")
                    file-name (.getName file)
                    proj (-> file-path (str/split #"/") first)
                    ]
               {:problem-rate problem-rate
                :total-nodes total-nodes
                :file-name file-name
                :file-ext (file-ext file-name)
                :file-path file-path
                :proj proj
               })))))
       (remove nil?)
       (maps-to-csv "tmp/parse_problem_rate_2.csv")
       ;(map prn)
       ))
