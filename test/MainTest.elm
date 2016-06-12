module MainTest exposing (..)

import ElmTest exposing (..)
import CollisionSpec


tests : Test
tests =
    suite "Eggdude"
        [ CollisionSpec.tests
        ]


main : Program Never
main =
    runSuiteHtml tests
