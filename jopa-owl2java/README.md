# OWL2Java

This application is a generator of object model/vocabulary file from an ontology.

When generating the object model, it can be configured to use all axioms in the provided ontology. Otherwise, it will
look for axioms annotated as integrity constraints - i.e. with
annotation "http://krizik.felk.cvut.cz/ontologies/2009/ic.owl#isIntegrityConstraintFor".

Possible configuration parameters are:

|       Parameter        |                           Default value                           | Explanation                                                                                                                         |
|:----------------------:|:-----------------------------------------------------------------:|-------------------------------------------------------------------------------------------------------------------------------------|
|           -m           |                                ""                                 | Path to a mapping file which allows resolving ontology IRIs to local files.                                                         |
|           -c           |                                ""                                 | Name of context specified by IC annotations. If specified, only axioms annotated with the correct context will be processed.        |
|           -p           |                            `generated`                            | Name of the package into which the vocabulary file and the model will be generated.                                                 |
|           -d           |                                ""                                 | Directory into which the target files (with package) will be generated.                                                             |
|           -i           |                              `false`                              | Whether to interpret all the axioms in the ontology as integrity constraints. If `true`, context is ignored.                        |
|           -w           |                              `false`                              | Whether to add OWLAPI IRIs for terms generated into the vocabulary file (normally, only String constants are generated).            |
|           -u           |                              `false`                              | Whether to add Java URIs for terms generated into the vocabulary file (normally, only String constants are generated).              |
|           -f           |                              `false`                              | Whether to ignore missing/failed ontology imports.                                                                                  |
|          -jca          | "http://krizik.felk.cvut.cz/ontologies/2009/ic.owl#javaClassName" | IRI of annotation used to specify Java class name to which the annotated OWL class is mapped. Relevant only for model generation.   |
|      -langStrings      |                              `true`                               | Whether to prefer generating string fields as `MultilingualString` when their range is langString.                                  |
|          -pt           |                             `string`                              | Type to use for `@Properties` value. Options are `String` and `Object` .                                                            |
|          -doc          |                              `true`                               | Whether to generate Javadoc using values of `rdfs:comment` axioms.                                                                  |
|          -ann          |                              `true`                               | Whether to automatically generate annotation fields corresponding to `rdfs:label` and `dc:description` for all entity classes.      |
|         -thing         |                              `true`                               | Whether to automatically generate an entity class corresponding to `owl:Thing`.                                                     |
|    -prefixProperty     |       `http://purl.org/vocab/vann/preferredNamespacePrefix`       | IRI of the annotation property whose value is used as the preferred prefix for an ontology when generating vocabulary and model.    |
|   -namespaceProperty   |        `http://purl.org/vocab/vann/preferredNamespaceUri`         | IRI of the annotation property whose value is used as the preferred namespace for an ontology when generating vocabulary and model. |
| -vocabularyUsePrefixes |                              `false`                              | Whether to always use the ontology prefix (when available) to shorten IRIs in the generated vocabulary file.                        |
|   -modelUsePrefixes    |                              `false`                              | Whether to always use the ontology prefix (when available) when generating entity class names in the model.                         |
|      -prefixFile       |                                ""                                 | Path to a file containing additional ontology prefix mappings (one `namespace=prefix` per line).                                    |

OWL2Java CLI prints help for all the supported tasks, including explanation of all options.

## Java Name Generation

Java name generation follows a set of rules that attempt to prevent name collisions and generate human-readable names.

### Vocabulary

This concerns generation of the `Vocabulary` class containing Java constants for all entities in the processed ontologies.

1. Ontology IRI generation:
   - All ontology IRI constants use the prefix `ONTOLOGY_IRI_`
   - If it is one of the well-known ontologies (such as SKOS, OWL, etc.), use a predefined name
   - Otherwise, extract the name from ontology IRI local name (i.e. the part after the last `/` or `#`)
   - If the field name is not unique and the ontology has a prefix, use the prefix
   - Otherwise, use the local name and append `_A` to disambiguate (repeatedly if necessary)
2. Entity constants generation
   - Classes have prefix `s_c`, properties have prefix `s_p`, individuals have prefix `s_i`
   - If `vocabularyUsePrefixes` is `true`, use the ontology prefix (when available)
   - Extract local name from IRI and append it to the prefix
   - If `vocabularyUsePrefixes` is `false` and the field is not unique, try using the ontology prefix (if available)
   - If the field is still not unique, append `_A` to disambiguate (repeatedly if necessary)

Examples:

```java
// Known ontology
public static final String ONTOLOGY_IRI_DC_TERMS = "http://purl.org/dc/terms/";
// Name extracted from IRI
public static final String ONTOLOGY_IRI_TERMIT = "http://onto.fel.cvut.cz/ontologies/application/termit";
public static final String ONTOLOGY_IRI_MODEL = "http://onto.fel.cvut.cz/ontologies/application/termit/model";
// Name extracted from IRI, not unique, but prefix is available
public static final String ONTOLOGY_IRI_DD_MODEL = "http://onto.fel.cvut.cz/ontologies/application/dd/model";
// Name extracted from IRI, not unique, and prefix is not available
public static final String ONTOLOGY_IRI_MODEL_A = "http://onto.fel.cvut.cz/ontologies/application/dd/model";
```

### Model

1. If `modelUsePrefixes` is `true`, use the ontology prefix (when available)
2. Extract entity local name from IRI and use it
3. If the name is not unique, try using the prefix (if available)
4. If the name is still not unique, append `_A` to disambiguate (repeatedly if necessary)


