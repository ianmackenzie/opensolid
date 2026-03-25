from pathlib import Path

from opensolid import Step

header = Step.header(
    description=["A minimal AP214 example with a single part"],
    implementation_level="2;1",
    file_name="demo",
    timestamp="2003-12-27T11:57:53",
    author=["Lothar Klein"],
    organization=["LKSoft"],
    preprocessor_version=" ",
    originating_system="IDA-STEP",
    authorization=" ",
    schema_identifiers=["AUTOMOTIVE_DESIGN { 1 0 10303 214 2 1 1}"],
)
application_context = Step.entity(
    "APPLICATION_CONTEXT", [Step.text("mechanical design")]
)
application_protocol_definition = Step.entity(
    "APPLICATION_PROTOCOL_DEFINITION",
    [
        Step.text(""),
        Step.text("automotive_design"),
        Step.int(2003),
        Step.reference_to(application_context),
    ],
)
product_context = Step.entity(
    "PRODUCT_CONTEXT",
    [
        Step.text(""),
        Step.reference_to(application_context),
        Step.text(""),
    ],
)
product = Step.entity(
    "PRODUCT",
    [
        Step.text("A0001"),
        Step.text("Test Part 1"),
        Step.text(""),
        Step.list([Step.reference_to(product_context)]),
    ],
)
product_definition_formation = Step.entity(
    "PRODUCT_DEFINITION_FORMATION",
    [Step.text("1"), Step.null, Step.reference_to(product)],
)
product_definition_context = Step.entity(
    "PRODUCT_DEFINITION_CONTEXT",
    [
        Step.text("part definition"),
        Step.reference_to(application_context),
        Step.text("manufacturing"),
    ],
)
organization = Step.entity(
    "ORGANIZATION", [Step.text("O0001"), Step.text("LKSoft"), Step.text("company")]
)
product_definition = Step.entity(
    "PRODUCT_DEFINITION",
    [
        Step.text("0"),
        Step.null,
        Step.reference_to(product_definition_formation),
        Step.reference_to(product_definition_context),
    ],
)
product_related_product_category = Step.entity(
    "PRODUCT_RELATED_PRODUCT_CATEGORY",
    [Step.text("part"), Step.null, Step.list([Step.reference_to(product)])],
)
applied_organization_assignment = Step.entity(
    "APPLIED_ORGANIZATION_ASSIGNMENT",
    [
        Step.reference_to(organization),
        Step.reference_to(Step.entity("ORGANIZATION_ROLE", [Step.text("id owner")])),
        Step.list([Step.reference_to(product)]),
    ],
)
model = Step.model(
    header,
    [
        application_context,
        application_protocol_definition,
        product_definition,
        product_related_product_category,
        applied_organization_assignment,
    ],
)
model.write(str(Path(__file__).parent / "file.step"))
