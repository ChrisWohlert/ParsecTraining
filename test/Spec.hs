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
\        private readonly DatabaseDataContext _context = new DatabaseDataContext();\n\
\        private readonly IBruger[] _bruger;\n\
\        //Some comment\n\
\        private readonly ITolkeBrugerQuery [] _tolkeBrugerQuery;\n\
\\n\
\        public string AProperty { get; set; }\n\
\\n\
\        public Sag/*SomeComment\n\nSomeMoreMultilineComment*/Query(DatabaseDataContext context, IBruger bruger, ITolkeBrugerQuery tolkeBrugerQuery)\n\
\        {\n\
\            _context = context;\n\
\            _bruger = bruger;\n\
\            _tolkeBrugerQuery = tolkeBrugerQuery;\n\
\        }\n\
\\n\
\        public static void Get<T>(T id)\n\
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
    describe "Lib.run_parseType" $ do
      it "returns a Class from a .cs file" $ do
        run_parseType someClass "(Test)" `shouldBe` Right (Class { class_usings = ["System","System.Collections.Generic","System.Security","Core.Constants","Core.Models.Shared","Core.Queries","Core.Queries.Shared","Core.User","DataAccess.Bruger","DataAccess.Database","System.Linq","AutoMapper.QueryableExtensions"]
                                                                  , class_namespace = "DataAccess.Queries.Shared"
                                                                  , class_visibility = Public
                                                                  , class_safe = Safe
                                                                  , class_isInterface = False
                                                                  , class_name = GenericClassName "SagQuery" "T"
                                                                  , class_abstract = True
                                                                  , class_baseClasses = [Single "ISagQuery", Single "ISomeOtherInterface"]
                                                                  , class_constraints = "T : struct, new"
                                                                  , class_members = [ Property Nothing (Single "DatabaseDataContext") (PropertyName "_context") Nothing "new DatabaseDataContext()" Private Readonly NonStatic []
                                                                                    , Property Nothing (List (Single "IBruger")) (PropertyName "_bruger") Nothing "" Private Readonly NonStatic []
                                                                                    , Property Nothing (List (Single "ITolkeBrugerQuery")) (PropertyName "_tolkeBrugerQuery") Nothing "" Private Readonly NonStatic []
                                                                                    , Property Nothing (Single "string") (PropertyName "AProperty") (Just (GetSet " get; set; ")) "" Public Mutable NonStatic []
                                                                                    , Constructor Public [Parameter NoRef NoParams (Single "DatabaseDataContext") "context" Nothing False
                                                                                      , Parameter NoRef NoParams (Single "IBruger") "bruger" Nothing False
                                                                                      , Parameter NoRef NoParams (Single "ITolkeBrugerQuery") "tolkeBrugerQuery" Nothing False] Nothing "{\n            _context = context;\n            _bruger = bruger;\n            _tolkeBrugerQuery = tolkeBrugerQuery;\n        }"
                                                                                    , Method (Concrete (MethodSignature Public Static Nothing (Single "void") (GenericMethodName "Get" "T") [Parameter NoRef NoParams (Single "T") "id" Nothing False] "" []) "{\n        }")
                                                                                    , Method (Abstract (MethodSignature Public NonStatic Nothing (Single "SagModel") (MethodName "Get") [Parameter NoRef NoParams (Single "int") "id" Nothing False] "" []))
                                                                                    , Method (Concrete (MethodSignature Public NonStatic Nothing (List (Single "SagModel")) (MethodName "GetAfsluttedeSagerForMyndighed") [Parameter NoRef NoParams (Single "int") "myndighedId" Nothing False] "" []) "{\n            Console.writeline(\"ASDASD\");\n        }")]
                                                                  , class_attributes = ["Attribute(=\"ClassAttribute\")"]})
                                                
    describe "Lib.parseProperty -> single property" $ do
      it "returns a single" $ do
        run_test parseProperty "private static readonly Datatype _name;" `shouldBe` (Right (Property Nothing (Single "Datatype") (PropertyName "_name") Nothing "" Private Readonly Static []))

    describe "Lib.parseProperty -> single Property with get set" $ do
      it "returns a single" $ do
        run_test parseProperty "public string AProperty { get; set; }" `shouldBe` (Right (Property Nothing (Single "string") (PropertyName "AProperty") (Just (GetSet " get; set; ")) "" Public Mutable NonStatic []))

    describe "Lib.parseProperty -> Array property" $ do
      it "returns a list" $ do
        run_test parseProperty "private static readonly Datatype[] _name;" `shouldBe` (Right (Property Nothing (List (Single "Datatype")) (PropertyName "_name") Nothing "" Private Readonly Static []))

    describe "Lib.parseProperty -> Array Property with space" $ do
      it "returns a list" $ do
        run_test parseProperty "private static readonly Datatype [] _name;" `shouldBe` (Right (Property Nothing (List (Single "Datatype")) (PropertyName "_name") Nothing "" Private Readonly Static []))

    describe "Lib.parseProperty -> Array Property with attribute" $ do
      it "returns a single with attribute" $ do
        run_test parseProperty "[Attribute(\"Name=Test\")]\n[Test] private static readonly Datatype _name;" `shouldBe` (Right (Property Nothing (Single "Datatype") (PropertyName "_name") Nothing "" Private Readonly Static ["Attribute(\"Name=Test\")", "Test"]))

    describe "Lib.parseProperty -> Property multiple declarations" $ do
      it "returns a Single with long name" $ do
        run_test parseProperty "[Attribute(\"Name=Test\")] private Datatype _name, _name2, _name3;" `shouldBe` (Right (Property Nothing (Single "Datatype") (MultiName ["_name", "_name2", "_name3"]) Nothing "" Private Mutable NonStatic ["Attribute(\"Name=Test\")"]))

    describe "Lib.parseMember -> Property with curly on new line" $ do
      it "returns property" $ do
        run_test parseMembers "public CudaContext CudaContext\n{\nget => this._cudaContext;\nprivate set => this._cudaContext = value;\n}}" `shouldBe` (Right [(Property Nothing (Single "CudaContext") (PropertyName "CudaContext") (Just (GetSet "\nget => this._cudaContext;\nprivate set => this._cudaContext = value;\n")) "" Public Mutable NonStatic [])])

    describe "Lib.parseAbstractMethod -> abstract method" $ do
      it "returns abstract method" $ do
        run_test parseAbstractMethod "public abstract SagModel Get(int id);" `shouldBe` (Right (Method (Abstract (MethodSignature Public NonStatic Nothing (Single "SagModel") (MethodName "Get") [Parameter NoRef NoParams (Single "int") "id" Nothing False] "" []))))

    describe "Lib.parseAbstractMethod -> generic method" $ do
      it "returns generic method" $ do
        run_test parseAbstractMethod "public abstract SagModel Get<T>(int id);" `shouldBe` (Right (Method (Abstract (MethodSignature Public NonStatic Nothing (Single "SagModel") (GenericMethodName "Get" "T") [Parameter NoRef NoParams (Single "int") "id" Nothing False] "" []))))

    describe "Lib.parseAbstractMethod -> generic returntype method" $ do
      it "returns generic returntype method" $ do
        run_test parseAbstractMethod "public abstract SagModel<T> Get(int id);" `shouldBe` (Right (Method (Abstract (MethodSignature Public NonStatic Nothing (Generic (Single "SagModel") [(Single "T")]) (MethodName "Get") [Parameter NoRef NoParams (Single "int") "id" Nothing False] "" []))))

    describe "Lib.parseMethod -> method with attribute" $ do
      it "returns method with attribute" $ do
        run_test parseMethod "[Attribute(Name = \"Test\")] public abstract SagModel Get(int id);" `shouldBe` (Right (Method (Abstract (MethodSignature Public NonStatic Nothing (Single "SagModel") (MethodName "Get") [Parameter NoRef NoParams (Single "int") "id" Nothing False] "" ["Attribute(Name = \"Test\")"]))))

    describe "Lib.parseMethod -> method without Parameter NoRef NoParamss" $ do
      it "returns method without Parameter NoRef NoParamss" $ do
        run_test parseMethod "public SagModel Get(){ something }" `shouldBe` (Right (Method (Concrete (MethodSignature Public NonStatic Nothing (Single "SagModel") (MethodName "Get") [] "" []) "{ something }")))

    describe "Lib.parseMethod -> method without Parameter NoRef NoParamss and with attribute" $ do
      it "returns method without Parameter NoRef NoParamss" $ do
        run_test parseMethod "[Test] public SagModel Get2Got(){ something }" `shouldBe` (Right (Method (Concrete (MethodSignature Public NonStatic Nothing (Single "SagModel") (MethodName "Get2Got") [] "" ["Test"]) "{ something }")))

    describe "Lib.parseMethod -> method without Parameter NoRef NoParamss with curlys" $ do
      it "returns method without Parameter NoRef NoParamss" $ do
        run_test parseConcreteMethod "public SagModel Get(){ for(...) { } test }" `shouldBe` (Right (Method (Concrete (MethodSignature Public NonStatic Nothing (Single "SagModel") (MethodName "Get") [] "" []) "{ for(...) { } test }")))

    describe "Lib.parseMethod -> method with T return type and constraints" $ do
      it "returns method" $ do
        run_test parseConcreteMethod "public static T Get<T>(object o) where T : struct, IEquatable<T>, IFormattable{}" `shouldBe` (Right (Method (Concrete (MethodSignature Public Static Nothing (Single "T") (GenericMethodName "Get" "T") [(Parameter NoRef NoParams (Single "object") "o" Nothing False)] "T : struct, IEquatable<T>, IFormattable" []) "{}")))

    describe "Lib.parseMethod -> method with out parameter" $ do
      it "returns method" $ do
        run_test parseConcreteMethod "public static string Get(out string a){}" `shouldBe` (Right (Method (Concrete (MethodSignature Public Static Nothing (Single "string") (MethodName "Get") [(Parameter Out NoParams (Single "string") "a" Nothing False)] "" []) "{}")))

    describe "Lib.parseContent -> content with multi curlys" $ do
      it "returns all content" $ do
        run_test (parseContent) "{ for{{}} }" `shouldBe` (Right "{ for{{}} }")

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
\        }" `shouldBe` (Right (Constructor Public [Parameter NoRef NoParams (Single "DatabaseDataContext") "context" Nothing False
                                                 , Parameter NoRef NoParams (Single "IBruger") "bruger" Nothing False
                                                 , Parameter NoRef NoParams (Single "ITolkeBrugerQuery") "tolkeBrugerQuery" Nothing False] Nothing "{\n            _context = context;\n            _bruger = bruger;\n            _tolkeBrugerQuery = tolkeBrugerQuery;\n        }"))

    describe "Lib.parseConstructor -> constructor with this : (test)" $ do
      it "return CtorCall" $ do
        run_test parseConstructor "public SagQuery(IBruger bruger) : this(bruger)\n\
\        {}" `shouldBe` (Right (Constructor Public [Parameter NoRef NoParams (Single "IBruger") "bruger" Nothing False] (Just (CtorCall "this" "bruger")) "{}"))

    describe "Lib.parseContent -> content with multi curlys" $ do
      it "returns all content" $ do
         run_test (parseContent) "\
\{\n\
\    var left = this.NewVolume(new[] { 1.0, 2.0, 3.0 }, new Shape(3));\n\
\    var right = this.NewVolume(new[] { 1.0, 2.0, 3.0 }, new Shape(3));\n\
\    var result = BuilderInstance<T>.Volume.SameAs(new Shape(3));\n\
\\n\
\    left.Add(right, result);\n\
\    AssertNumber.AreEqual(2.0, result.Get(0));\n\
\    AssertNumber.AreEqual(4.0, result.Get(1));\n\
\    AssertNumber.AreEqual(6.0, result.Get(2));\n\
\}" `shouldBe` (Right ("\
\{\n\
\    var left = this.NewVolume(new[] { 1.0, 2.0, 3.0 }, new Shape(3));\n\
\    var right = this.NewVolume(new[] { 1.0, 2.0, 3.0 }, new Shape(3));\n\
\    var result = BuilderInstance<T>.Volume.SameAs(new Shape(3));\n\
\\n\
\    left.Add(right, result);\n\
\    AssertNumber.AreEqual(2.0, result.Get(0));\n\
\    AssertNumber.AreEqual(4.0, result.Get(1));\n\
\    AssertNumber.AreEqual(6.0, result.Get(2));\n\
\}"))

    describe "Lib.parseConcrete -> method default Parameter NoRef NoParams" $ do
      it "returns method" $ do
        run_test parseConcreteMethod "public static void AreEqual<T>(double delta = 0){ for(...) { } test }" `shouldBe` (Right (Method (Concrete (MethodSignature Public Static Nothing (Single "void") (GenericMethodName "AreEqual" "T") [(Parameter NoRef NoParams (Single "double") "delta" (Just "0") False)] "" []) "{ for(...) { } test }")))

    describe "Lib.parseConcrete -> extension method" $ do
      it "returns method" $ do
        run_test parseConcreteMethod "public static void AreEqual<T>(this double delta){ for(...) { } test }" `shouldBe` (Right (Method (Concrete (MethodSignature Public Static Nothing (Single "void") (GenericMethodName "AreEqual" "T") [(Parameter NoRef NoParams (Single "double") "delta" Nothing True)] "" []) "{ for(...) { } test }")))

    describe "Lib.parseEnum -> enum" $ do
      it "returns enum" $ do
        run_test (parseEnum [] []) "public enum DataLocation\n\
                                      \{\n\
                                      \    Host,\n\
                                      \    Device\n\
                                      \}" `shouldBe` (Right (Enum [] [] Public "DataLocation" ["Host", "Device"] []))
                                      
    describe "Lib.parseOperatorOverload -> Operator overloading method" $ do
      it "returns method" $ do
        run_test parseOperatorOverload "public static implicit operator Volume<Key>(T<k> t, T<k> l) { }" `shouldBe` (Right (Method (OperatorOverload Implicit Public (GenericMethodName "Volume" "Key") [ (Parameter NoRef NoParams (Generic (Single "T") [(Single "k")]) "t" Nothing False), (Parameter NoRef NoParams (Generic (Single "T") [(Single "k")]) "l" Nothing False)] "" [] "{ }")))
                                      
    describe "Lib.parseOperatorOverload -> Operator overloading method" $ do
      it "returns method" $ do
        run_test parseOperatorOverload "public static Datatype operator +(T<k> t, T<k> l) { }" `shouldBe` (Right (Method (OperatorOverload (Unary (Single "Datatype")) Public (MethodName "+") [ (Parameter NoRef NoParams (Generic (Single "T") [(Single "k")]) "t" Nothing False), (Parameter NoRef NoParams (Generic (Single "T") [(Single "k")]) "l" Nothing False)] "" [] "{ }")))
                                      
    describe "Lib.parseClass -> Class with class" $ do
      it "returns Class with a class member" $ do
        run_test (parseClass [] []) "public class Test { \nprivate string _test;\n private class Inner { public Inner(){} } }" `shouldBe` (Right (Class [] [] Public Safe False False (ClassName "Test") [] "" [ (Property Nothing (Single "string") (PropertyName "_test") Nothing "" Private Mutable NonStatic [])
                                                                                                                                                                                                           , (InnerType (Class [] [] Private Safe False False (ClassName "Inner") [] "" [(Constructor Public [] Nothing "{}")] []))] []))
                                      

    describe "Lib.parseEvent -> event" $ do
      it "returns event" $ do
        run_test parseMember "public event Datatype name;" `shouldBe` (Right (Event Public (Single "Datatype") "name"))

run_test rule input = run_parse rule input "(Test)"