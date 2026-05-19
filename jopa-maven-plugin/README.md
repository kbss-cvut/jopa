# JOPA Maven Plugin

The JOPA Maven plugin provides object model-related generators.

## OWL2Java Mojo

The OWL2Java Mojo can generate an object model based on the specified ontology. Or, if object model generation is
too much, it can generate a `Vocabulary` file containing all classes and properties found in the ontology, so that
they can be used in the mapping annotations in the object model.

Internally, the plugin uses [OWL2Java](../jopa-owl2java).

### Goals

- `owl2java-transform` - run the OWL2Java transformation

### Phase

- JOPA Maven plugin is by default executed during the `generate-sources` build phase.

### Configuration

| Parameter                        |                     Default value                     | Description                                                                                                                                    |
|:---------------------------------|:-----------------------------------------------------:|:-----------------------------------------------------------------------------------------------------------------------------------------------|
| `mapping-file`                   |                          ""                           | Path to the mapping file which allows resolving ontology IRIs to local files.                                                                  |
| `package`                        |                          ""                           | Name of the target package into which the model/vocabulary should be generated.                                                                |
| `context-name`                   |                          ""                           | Name of context specified by integrity constraint annotations. If specified, only axioms annotated with the correct context will be processed. |
| `ontology-iri`                   |                          ""                           | IRI of the ontology that should be processed.                                                                                                  |
| `output-directory`               |    `${project.basedir}/src/main/generated-sources`    | Target directory into which the model/vocabulary should be generated.                                                                          |
| `with-owlapi`                    |                        `false`                        | Whether to also generate OWLAPI IRIs for the generated vocabulary terms.                                                                       |
| `with-java-uris`                 |                        `false`                        | Whether to also generate Java URIs for the generated vocabulary terms.                                                                         |
| `whole-ontology-as-ics`          |                        `false`                        | Whether to use the whole ontology for generation. If specified, `context-name` is ignored.                                                     |
| `vocabulary-only`                |                        `false`                        | Whether to generate only the vocabulary file or the object model as well.                                                                      |
| `ignore-failed-imports`          |                        `false`                        | Whether to ignore failed ontology imports (owl:import).                                                                                        |
| `properties-type`                |                       `string`                        | Java type to use as value in the unmapped properties attribute definition. Options are `string` or `object`.                                   |
| `javadoc-from-rdfs-comment`      |                        `true`                         | Whether to use RDFS comment annotations to generate Javadoc for classes and attributes.                                                        |
| `prefer-multilingual-strings`    |                        `true`                         | Whether to use `MultilingualString` type for fields whose property range is langString.                                                        |
| `generate-annotation-fields`     |                        `true`                         | Whether to generate annotation fields (name, description) for all entities.                                                                    |
| `generate-thing`                 |                        `true`                         | Whether to generate an entity classes corresponding to `owl:Thing`.                                                                            |
| `ontology-prefix-property`       | `http://purl.org/vocab/vann/preferredNamespacePrefix` | IRI of the annotation property whose value is used as the preferred prefix for an ontology when generating vocabulary and model.               |
| `ontology-namespace-property`    |  `http://purl.org/vocab/vann/preferredNamespaceUri`   | IRI of the annotation property whose value is used as the preferred namespace for an ontology when generating vocabulary and model.            |
| `vocabulary-always-use-prefixes` |                        `true`                         | Whether to always use the ontology prefix (when available) to shorten IRIs in the generated vocabulary file.                                   |
| `model-always-use-prefixes`      |                        `false`                        | Whether to always use the ontology prefix (when available) when generating entity class names in the model.                                    |
| `prefix-mapping-file`            |                          ""                           | Path to a file containing additional ontology prefix mappings (one `namespace=prefix` per line).                                               |

## ModelGen Mojo

The ModelGen Mojo can generate static metamodel classes based on the application object model. Static metamodel can be
used, for example,
when creating Criteria API queries, building Descriptors for entity manager operations etc.

Internally, it uses the [ModelGen](../modelgen) module.

### Goals

- `modelgen` - run the static metamodel generation

### Phase

JOPA Maven plugin is by default executed during the `generate-sources` build phase.

### Configuration

| Parameter              |                 Default value                 | Description                                                                                                                                                                                   |
|:-----------------------|:---------------------------------------------:|:----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `output-directory`     | `./target/generated-sources/static-metamodel` | Target directory into which the generated static metamodel classes should be written.                                                                                                         |
| `source-package`       |                      ""                       | If set, only entity classes whose fully qualified name contains the specified package are processed.                                                                                          |
| `additional-sources`   |                      ""                       | Additional source directory to process besides the project's compile source roots.                                                                                                            |
| `debug-option`         |                    `false`                    | Whether to log additional debug messages during static metamodel generation.                                                                                                                  |
| `output-property-iris` |                    `false`                    | Whether to generate constants holding IRIs of mapped properties in the generated metamodel classes.                                                                                           |
| `output-iri-as-string` |                    `false`                    | Whether the generated property IRI constants should be of type `String` instead of `cz.cvut.kbss.jopa.model.IRI`.                                                                             |
| `initialize-iris`      |                    `false`                    | Whether the generated property IRI constants should be initialized with their values (otherwise they are declared without value and initialized by JOPA when the runtime metamodel is built). |

