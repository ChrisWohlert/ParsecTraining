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
                                                                              , Parameter (Single "ITolkeBrugerQuery") "tolkeBrugerQuery"] "\n            _context = context;\n            _bruger = bruger;\n            _tolkeBrugerQuery = tolkeBrugerQuery;\n        "
                                                                              , Method Public (Single "SagModel") "Get" [Parameter (Single "int") "id"] "\n        "
                                                                              , Method Public (List (Single "SagModel")) "GetAfsluttedeSagerForMyndighed" [Parameter (Single "int") "myndighedId"] "\n            Console.writeline(\"ASDASD\");\n        "]
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


run_test rule input = run_parse rule input "(Test)"