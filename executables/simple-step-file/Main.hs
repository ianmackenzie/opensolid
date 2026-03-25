module Main (main) where

import OpenSolid.Prelude
import OpenSolid.Step qualified as Step

main :: IO ()
main = do
  let header =
        Step.header
          (#description ["A minimal AP214 example with a single part"])
          (#implementationLevel "2;1")
          (#fileName "demo")
          (#timestamp "2003-12-27T11:57:53")
          (#author ["Lothar Klein"])
          (#organization ["LKSoft"])
          (#preprocessorVersion " ")
          (#originatingSystem "IDA-STEP")
          (#authorization " ")
          (#schemaIdentifiers ["AUTOMOTIVE_DESIGN { 1 0 10303 214 2 1 1}"])
  let applicationContext = Step.entity "APPLICATION_CONTEXT" [Step.text "mechanical design"]
  let applicationProtocolDefinition =
        Step.entity "APPLICATION_PROTOCOL_DEFINITION" $
          [ Step.text ""
          , Step.text "automotive_design"
          , Step.int 2003
          , Step.referenceTo applicationContext
          ]
  let product =
        Step.entity "PRODUCT" $
          [ Step.text "A0001"
          , Step.text "Test Part 1"
          , Step.text ""
          , Step.list Step.referenceTo $
              [ Step.entity "PRODUCT_CONTEXT" $
                  [ Step.text ""
                  , Step.referenceTo applicationContext
                  , Step.text ""
                  ]
              ]
          ]
  let productDefinition =
        Step.entity "PRODUCT_DEFINITION" $
          [ Step.text "0"
          , Step.null
          , Step.referenceTo $
              Step.entity "PRODUCT_DEFINITION_FORMATION" $
                [ Step.text "1"
                , Step.null
                , Step.referenceTo product
                ]
          , Step.referenceTo $
              Step.entity "PRODUCT_DEFINITION_CONTEXT" $
                [ Step.text "part definition"
                , Step.referenceTo applicationContext
                , Step.text "manufacturing"
                ]
          ]
  let productRelatedProductCategory =
        Step.entity "PRODUCT_RELATED_PRODUCT_CATEGORY" $
          [ Step.text "part"
          , Step.null
          , Step.list Step.referenceTo [product]
          ]
  let appliedOrganizationAssignment =
        Step.entity "APPLIED_ORGANIZATION_ASSIGNMENT" $
          [ Step.referenceTo $
              Step.entity "ORGANIZATION" $
                [ Step.text "O0001"
                , Step.text "LKSoft"
                , Step.text "company"
                ]
          , Step.referenceTo $
              Step.entity "ORGANIZATION_ROLE" [Step.text "id owner"]
          , Step.list Step.referenceTo [product]
          ]
  Step.write "executables/simple-step-file/file.step" header $
    [ applicationContext
    , applicationProtocolDefinition
    , productDefinition
    , productRelatedProductCategory
    , appliedOrganizationAssignment
    ]
