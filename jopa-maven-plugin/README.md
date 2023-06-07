# JOPA Maven Plugin

JOPA Maven plugin can generate an object model based on the specified ontology. Or, if object model generation is
too much, it can generate a `Vocabulary` file containing all classes and properties found in the ontology, so that
they can be used in the mapping annotations in the object model.

Internally, the plugin uses [OWL2Java](../jopa-owl2java).

## Goals

- `owl2java-transform` - run the OWL2Java transformation

## Phase

- JOPA Maven plugin is by default executed during the `generate-sources` build phase.

## Configuration

| Parameter                     |                  Default value                  | Description                                                                                                                                    |
|:------------------------------|:-----------------------------------------------:|:-----------------------------------------------------------------------------------------------------------------------------------------------|
| `mapping-file`                |                       ""                        | Path to the mapping file which allows resolving ontology IRIs to local files.                                                                  |
| `package`                     |                       ""                        | Name of the target package into which the model/vocabulary should be generated.                                                                |
| `context-name`                |                       ""                        | Name of context specified by integrity constraint annotations. If specified, only axioms annotated with the correct context will be processed. |
| `ontology-iri`                |                       ""                        | IRI of the ontology that should be processed.                                                                                                  |
| `output-directory`            | `${project.basedir}/src/main/generated-sources` | Target directory into which the model/vocabulary should be generated.                                                                          |
| `with-owlapi`                 |                     `false`                     | Whether to also generate OWLAPI IRIs for the generated vocabulary terms.                                                                       |
| `whole-ontology-as-ics`       |                     `false`                     | Whether to use the whole ontology for generation. If specified, `context-name` is ignored.                                                     |
| `vocabulary-only`             |                     `false`                     | Whether to generate only the vocabulary file or the object model as well.                                                                      |
| `ignore-failed-imports`       |                     `false`                     | Whether to ignore failed ontology imports (owl:import).                                                                                        |
| `properties-type`             |                    `string`                     | Java type to use as value in the unmapped properties attribute definition. Options are `string` or `object`.                                   |
| `javadoc-from-rdfs-comment`   |                     `true`                      | Whether to use RFDS comment annotations to generate Javadoc for classes and attributes.                                                        |
| `prefer-multilingual-strings` |                     `true`                      | Whether to use `MultilingualString` type for fields whose property range is langString.                                                        |
| `generate-annotation-fields`  |                     `true`                      | Whether to generate annotation fields (name, description) for all entities.                                                                    |
| `generate-thing`              |                     `true`                      | Whether to generate an entity classes corresponding to `owl:Thing`.                                                                            |

