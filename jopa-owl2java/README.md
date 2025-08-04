# OWL2Java

This application is a generator of object model/vocabulary file from an ontology.

When generating the object model, it can be configured to use all axioms in the provided ontology. Otherwise, it will
look for axioms annotated as integrity constraints - i.e. with
annotation "http://krizik.felk.cvut.cz/ontologies/2009/ic.owl#isIntegrityConstraintFor".

Possible configuration parameters are:

|  Parameter   |                           Default value                           | Explanation                                                                                                                       |
|:------------:|:-----------------------------------------------------------------:|-----------------------------------------------------------------------------------------------------------------------------------|
|      -m      |                                ""                                 | Path to a mapping file which allows resolving ontology IRIs to local files.                                                       |
|      -c      |                                ""                                 | Name of context specified by IC annotations. If specified, only axioms annotated with the correct context will be processed.      |
|      -p      |                            `generated`                            | Name of the package into which the vocabulary file and the model will be generated.                                               |
|      -d      |                                ""                                 | Directory into which the target files (with package) will be generated.                                                           |
|      -i      |                              `false`                              | Whether to interpret all the axioms in the ontology as integrity constraints. If `true`, context is ignored.                      |
|      -w      |                              `false`                              | Whether to add OWLAPI IRIs for terms generated into the vocabulary file (normally, only String constants are generated).          |
|      -u      |                              `false`                              | Whether to add Java URIs for terms generated into the vocabulary file (normally, only String constants are generated).            |
|      -f      |                              `false`                              | Whether to ignore missing/failed ontology imports.                                                                                |
|     -jca     | "http://krizik.felk.cvut.cz/ontologies/2009/ic.owl#javaClassName" | IRI of annotation used to specify Java class name to which the annotated OWL class is mapped. Relevant only for model generation. |
| -langStrings |                              `true`                               | Whether to prefer generating string fields as `MultilingualString` when their range is langString.                                |
|     -pt      |                             `string`                              | Type to use for `@Properties` value. Options are `String` and `Object` .                                                          |
|     -doc     |                              `true`                               | Whether to generate Javadoc using values of `rdfs:comment` axioms.                                                                |
|     -ann     |                              `true`                               | Whether to automatically generate annotation fields corresponding to `rdfs:label` and `dc:description` for all entity classes.    |
|    -thing    |                              `true`                               | Whether to automatically generate an entity class corresponding to `owl:Thing`.                                                   |

OWL2Java CLI prints help for all the supported tasks, including explanation of all options.
