import Lib
import Class
import Test.Hspec

someClass = "using System;\n\ 
\using System.Collections.Generic;\n\ 
\using System.Security;\n\
\using Core.Constants;\n\
\using Core.Models.Shared;\n\
\using Core.Queries;\n\
\using Core.Queries.Shared;\n\
\using Core.User;\n\
\using DataAccess.Bruger;\n\
\using DataAccess.Database;\n\
\using System.Linq;\n\
\using AutoMapper.QueryableExtensions;\n\
\\n\
\namespace DataAccess.Queries.Shared\n\
\{\n\
\    [Attribute(=\"ClassAttribute\")]\n\
\    public abstract class SagQuery<T> : ISagQuery, ISomeOtherInterface where T : struct, new\n\
\    {\n\
\        private readonly DatabaseDataContext _context;\n\
\        private readonly IBruger[] _bruger;\n\
\        //Some comment\n\
\        private readonly ITolkeBrugerQuery [] _tolkeBrugerQuery;\n\
\\n\
\        public Sag/*SomeComment\n\nSomeMoreMultilineComment*/Query(DatabaseDataContext context, IBruger bruger, ITolkeBrugerQuery tolkeBrugerQuery)\n\
\        {\n\
\            _context = context;\n\
\            _bruger = bruger;\n\
\            _tolkeBrugerQuery = tolkeBrugerQuery;\n\
\        }\n\
\\n\
\        public SagModel Get(int id)\n\
\        {\n\
\        }\n\
\\n\
\        public abstract SagModel Get(int id);\n\
\\n\
\        public List<SagModel> GetAfsluttedeSagerForMyndighed(int myndighedId)\n\
\        {\n\
\            Console.writeline(\"ASDASD\");\n\
\        }\n\
\    }\n\
\}"

main :: IO ()
main = hspec $ do
    describe "Lib.run_parseClass" $ do
      it "returns a Class from a .cs file" $ do
        run_parseClass someClass "(Test)" `shouldBe` Right (Class { usings = ["System","System.Collections.Generic","System.Security","Core.Constants","Core.Models.Shared","Core.Queries","Core.Queries.Shared","Core.User","DataAccess.Bruger","DataAccess.Database","System.Linq","AutoMapper.QueryableExtensions"]
                                                                  , namespace = "DataAccess.Queries.Shared", visibility = Public
                                                                  , className = GenericClassName "SagQuery" "T"
                                                                  , abstract = True
                                                                  , baseClasses = ["ISagQuery","ISomeOtherInterface"]
                                                                  , constraints = "T : struct, new"
                                                                  , members = [ Property (Single "DatabaseDataContext") "_context" "" Private Readonly NonStatic []
                                                                              , Property (List (Single "IBruger")) "_bruger" "" Private Readonly NonStatic []
                                                                              , Property (List (Single "ITolkeBrugerQuery")) "_tolkeBrugerQuery" "" Private Readonly NonStatic []
                                                                              , Constructor Public [Parameter (Single "DatabaseDataContext") "context"
                                                                                , Parameter (Single "IBruger") "bruger"
                                                                                , Parameter (Single "ITolkeBrugerQuery") "tolkeBrugerQuery"] "{\n            _context = context;\n            _bruger = bruger;\n            _tolkeBrugerQuery = tolkeBrugerQuery;\n        }"
                                                                              , Method (Concrete (MethodSignature Public (Single "SagModel") (MethodName "Get") [Parameter (Single "int") "id"] []) "{\n        }")
                                                                              , Method (Abstract (MethodSignature Public (Single "SagModel") (MethodName "Get") [Parameter (Single "int") "id"] []))
                                                                              , Method (Concrete (MethodSignature Public (List (Single "SagModel")) (MethodName "GetAfsluttedeSagerForMyndighed") [Parameter (Single "int") "myndighedId"] []) "{\n            Console.writeline(\"ASDASD\");\n        }")]
                                                                  , attributes = ["Attribute(=\"ClassAttribute\")"]})
                                                
    describe "Lib.parseProperty -> single property" $ do
      it "returns a single" $ do
        run_test parseProperty "private static readonly Datatype _name;" `shouldBe` (Right (Property (Single "Datatype") "_name" "" Private Readonly Static []))

    describe "Lib.parseProperty -> Array property" $ do
      it "returns a list" $ do
        run_test parseProperty "private static readonly Datatype[] _name;" `shouldBe` (Right (Property (List (Single "Datatype")) "_name" "" Private Readonly Static []))

    describe "Lib.parseProperty -> Array property with space" $ do
      it "returns a list" $ do
        run_test parseProperty "private static readonly Datatype [] _name;" `shouldBe` (Right (Property (List (Single "Datatype")) "_name" "" Private Readonly Static []))

    describe "Lib.parseProperty -> Array property with attribute" $ do
      it "returns a single with attribute" $ do
        run_test parseProperty "[Attribute(\"Name=Test\")] private static readonly Datatype _name;" `shouldBe` (Right (Property (Single "Datatype") "_name" "" Private Readonly Static ["Attribute(\"Name=Test\")"]))

    describe "Lib.parseAbstractMethod -> abstract method" $ do
      it "returns abstract method" $ do
        run_test parseAbstractMethod "public abstract SagModel Get(int id);" `shouldBe` (Right (Method (Abstract (MethodSignature Public (Single "SagModel") (MethodName "Get") [Parameter (Single "int") "id"] []))))

    describe "Lib.parseAbstractMethod -> generic method" $ do
      it "returns generic method" $ do
        run_test parseAbstractMethod "public abstract SagModel Get<T>(int id);" `shouldBe` (Right (Method (Abstract (MethodSignature Public (Single "SagModel") (GenericMethodName "Get" "T") [Parameter (Single "int") "id"] []))))

    describe "Lib.parseAbstractMethod -> generic returntype method" $ do
      it "returns generic returntype method" $ do
        run_test parseAbstractMethod "public abstract SagModel<T> Get(int id);" `shouldBe` (Right (Method (Abstract (MethodSignature Public (Generic (Single "SagModel") "T") (MethodName "Get") [Parameter (Single "int") "id"] []))))

    describe "Lib.parseMethod -> method with attribute" $ do
      it "returns method with attribute" $ do
        run_test parseMethod "[Attribute(Name = \"Test\")] public abstract SagModel Get(int id);" `shouldBe` (Right (Method (Abstract (MethodSignature Public (Single "SagModel") (MethodName "Get") [Parameter (Single "int") "id"] ["Attribute(Name = \"Test\")"]))))

    describe "Lib.parseMethod -> method without parameters" $ do
      it "returns method without parameters" $ do
        run_test parseMethod "public SagModel Get(){ something }" `shouldBe` (Right (Method (Concrete (MethodSignature Public (Single "SagModel") (MethodName "Get") [] []) "{ something }")))

    describe "Lib.parseMethod -> method without parameters and with attribute" $ do
      it "returns method without parameters" $ do
        run_test parseMembers "[Test] public SagModel Get2Got(){ something }" `shouldBe` (Right [(Method (Concrete (MethodSignature Public (Single "SagModel") (MethodName "Get2Got") [] ["Test"]) "{ something }"))])

    describe "Lib.parseMethod -> method without parameters with curlys" $ do
      it "returns method without parameters" $ do
        run_test parseConcreteMethod "public SagModel Get(){ for(...) { } test }" `shouldBe` (Right (Method (Concrete (MethodSignature Public (Single "SagModel") (MethodName "Get") [] []) "{ for(...) { } test }")))

    describe "Lib.parseContent -> content with multi curlys" $ do
      it "returns all content" $ do
        run_test (parseContent 0) "{ for{{}}}" `shouldBe` (Right "{ for{{}}}")

    describe "Lib.parseConstructor -> constructor" $ do
      it "constructor" $ do
        run_test parseConstructor "public SagQuery(DatabaseDataContext context, IBruger bruger, ITolkeBrugerQuery tolkeBrugerQuery)\n\
\        {\n\
\            _context = context;\n\
\            _bruger = bruger;\n\
\            _tolkeBrugerQuery = tolkeBrugerQuery;\n\
\        }\n\
\\n\
\        public SagModel Get(int id)\n\
\        {\n\
\        }" `shouldBe` (Right (Constructor Public [Parameter (Single "DatabaseDataContext") "context"
                                                 , Parameter (Single "IBruger") "bruger"
                                                 , Parameter (Single "ITolkeBrugerQuery") "tolkeBrugerQuery"] "{\n            _context = context;\n            _bruger = bruger;\n            _tolkeBrugerQuery = tolkeBrugerQuery;\n        }"))


run_test rule input = run_parse rule input "(Test)"